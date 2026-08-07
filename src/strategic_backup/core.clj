(ns strategic-backup.core
  "Entry point and top-level orchestration.

   CLI subcommands:
     backup          — run the full backup pipeline
     restore-test    — run the automated restore verification test
       --skip-verify   forces Mode 3 (no checksum verification)"
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [strategic-backup.config :as config]
            [strategic-backup.db :as db]
            [strategic-backup.manifest :as manifest]
            [strategic-backup.pipeline :as pipeline]
            [strategic-backup.retention :as retention]
            [strategic-backup.restore :as restore]
            [strategic-backup.shell :as shell]
            [strategic-backup.snapshot :as snapshot]
            [strategic-backup.upload :as upload]
            [taoensso.timbre :as log])
  (:import (java.time Instant))
  (:gen-class))

;; ---------------------------------------------------------------------------
;; Backup pipeline
;; ---------------------------------------------------------------------------

(defn run-backup!
  "Orchestrate the full backup pipeline.

   Order:
     1.  load-config + validate-config + resolve-secrets
     2.  snapshot/create-snapshot!
     3.  manifest/compute-file-checksums
     4.  pipeline/run-pipeline!  (zfs send | gzip | openssl enc)
     5.  manifest/compute-stream-checksum
     6.  manifest/build-manifest
     7.  manifest/write-edn!
     8.  db/persist-manifest!    (fire-and-forget, does not abort on failure)
     9.  upload/rclone-copy!
     10. retention/enforce-retention!
     11. delete local archive
     12. manifest/prune-local-edns!

   Returns nil on success.
   Throws ex-info (with :stage context) on any pipeline failure."
  [config executor]
  (let [dataset        (:dataset config)
        test-dataset   (:test-dataset config)
        staging-dir    (:staging-dir config)
        b2-bucket      (:b2-bucket config)
        b2-path-prefix (:b2-path-prefix config)
        b2-remote      (str "b2:" b2-bucket "/" b2-path-prefix)
        retention-count (:retention-count config)
        local-retention (:local-manifest-retention config)
        compression    (:compression config)
        cipher         (:encryption-cipher config)
        snap-prefix    (:snapshot-prefix config)
        pg-conn-string (get-in config [:secrets :pg-conn-string])]

    ;; Validate retention count before doing any work
    (when (or (nil? retention-count) (<= retention-count 0))
      (throw (ex-info "retention-count must be a positive integer"
                      {:stage           :config
                       :retention-count retention-count})))

    (log/info "Starting backup" {:dataset dataset})

    ;; 1. Create snapshot
    (let [now           (.toString (Instant/now))
          snap-name     (snapshot/snapshot-name dataset snap-prefix now)
          _             (snapshot/create-snapshot! executor snap-name)

          ;; 2. Compute per-file checksums from dataset mountpoint
          ;; (mountpoint is inferred as /mnt/<dataset-slug> or just use dataset)
          mountpoint    (str "/" (str/replace dataset "/" "/"))
          file-checksums (manifest/compute-file-checksums executor mountpoint)

          ;; 3. Check ZFS encryption status
          zfs-enc?      (snapshot/zfs-encrypted? executor dataset)

          ;; 4. Build archive filename and run pipeline
          ts-compact    (str/replace (str/replace
                                      (subs now 0 19)
                                      "T" "T")
                                     ":" "")
          slug          (pipeline/dataset-slug dataset)
          archive-fname (pipeline/archive-filename slug ts-compact zfs-enc?)
          archive-path  (str staging-dir "/" archive-fname)
          send-cmd      (snapshot/send-stream-cmd snap-name)
          pipeline-cmd  (pipeline/build-pipeline-cmd
                         send-cmd compression zfs-enc? cipher archive-path)
          _             (pipeline/run-pipeline! executor pipeline-cmd)

          ;; 5. Compute stream checksum of the archive
          stream-checksum (manifest/compute-stream-checksum executor archive-path)

          ;; 6. Build manifest
          created-at    now
          the-manifest  (manifest/build-manifest
                         dataset snap-name stream-checksum created-at
                         zfs-enc? compression archive-fname file-checksums)

          ;; 7. Write manifest EDN to staging dir
          edn-path      (manifest/write-edn! the-manifest staging-dir)]

      ;; 8. Persist to PostgreSQL (fire-and-forget — never aborts backup)
      (future
        (try
          (db/persist-manifest! pg-conn-string the-manifest)
          (catch Exception e
            (log/warn "Async DB persist failed:" (.getMessage e)))))

      ;; 9. Upload archive to B2 (manifest EDN is NOT uploaded)
      (upload/rclone-copy! executor archive-path b2-remote)

      ;; 10. Apply retention policy
      (let [remote-files (upload/list-remote executor b2-remote)
            sorted       (retention/sort-by-timestamp remote-files)
            ret-result   (retention/enforce-retention! executor b2-remote sorted retention-count)]
        (when (seq (:failed ret-result))
          (log/warn "Some retention deletions failed:" (:failed ret-result))))

      ;; 11. Delete local archive (upload already succeeded)
      (let [f (io/file archive-path)]
        (if (.delete f)
          (log/info "Deleted local archive:" archive-path)
          (log/warn "Failed to delete local archive (non-fatal):" archive-path)))

      ;; 12. Prune old local EDN manifests
      (manifest/prune-local-edns! staging-dir local-retention)

      (log/info "Backup complete" {:snapshot snap-name :archive archive-fname})
      nil)))

;; ---------------------------------------------------------------------------
;; Restore test entry point
;; ---------------------------------------------------------------------------

(defn run-restore-test-cmd!
  "Entry point for the restore-test subcommand.

   Delegates to restore/run-restore-test! after loading and validating config."
  [config executor skip-verify?]
  (restore/run-restore-test! config executor skip-verify?))

;; ---------------------------------------------------------------------------
;; CLI entry point
;; ---------------------------------------------------------------------------

(defn -main
  "CLI dispatcher.

   Usage:
     strategic-backup backup
     strategic-backup restore-test [--skip-verify]"
  [& args]
  (let [subcommand   (first args)
        skip-verify? (some #{"--skip-verify"} (rest args))
        executor     (shell/make-default-executor)]
    (try
      (let [cfg (-> (config/load-config nil)
                    (config/validate-config)
                    (config/resolve-secrets))]
        (case subcommand
          "backup"
          (do (run-backup! cfg executor)
              (System/exit 0))

          "restore-test"
          (do (run-restore-test-cmd! cfg executor (boolean skip-verify?))
              (System/exit 0))

          (do (log/error "Unknown subcommand:" subcommand
                         "Valid subcommands: backup, restore-test")
              (System/exit 1))))
      (catch clojure.lang.ExceptionInfo e
        (log/error "Pipeline failed" {:stage   (:stage (ex-data e))
                                      :message (.getMessage e)
                                      :data    (ex-data e)})
        (System/exit 1))
      (catch Exception e
        (log/error "Unexpected error:" (.getMessage e))
        (System/exit 1)))))
