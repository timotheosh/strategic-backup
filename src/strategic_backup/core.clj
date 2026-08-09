(ns strategic-backup.core
  "Entry point and top-level orchestration.

   CLI subcommands:
     backup          — run the full backup pipeline
       --strict-checksums  abort on the first file-checksum failure,
                            instead of logging and excluding the file
     restore-test    — run the automated restore verification test
       --skip-verify   forces Mode 3 (no checksum verification)
     --db-test       — test the configured database connection and exit
     --b2-test       — test the configured B2/rclone connection and exit
     --checksums     — run only file-checksum computation against the
                        real dataset, in isolation, and exit
       --strict-checksums  same as above"
  (:require [clojure.java.io :as io]
            [strategic-backup.config :as config]
            [strategic-backup.db :as db]
            [strategic-backup.manifest :as manifest]
            [strategic-backup.pipeline :as pipeline]
            [strategic-backup.retention :as retention]
            [strategic-backup.restore :as restore]
            [strategic-backup.shell :as shell]
            [strategic-backup.snapshot :as snapshot]
            [strategic-backup.timing :as timing]
            [strategic-backup.upload :as upload]
            [taoensso.timbre :as log])
  (:import (java.time Instant))
  (:gen-class))

;; ---------------------------------------------------------------------------
;; Backup pipeline
;; ---------------------------------------------------------------------------

(def ^:private db-persist-timeout-ms
  "How long run-backup! waits for the async DB persist future (step 8/13)
   before giving up on it. System/exit does not wait for background
   threads, so without this bound, a slow-to-fail DB connection (e.g. an
   unreachable host taking a long time to time out) could get silently
   killed mid-attempt — including its own failure-logging catch block —
   whenever the rest of the backup pipeline finishes first."
  10000)

(defn await-db-persist!
  "Action. Blocks up to `timeout-ms` for `persist-future` (the async DB
   persist kicked off in run-backup!'s step 8) to finish, so a failure —
   or a timeout itself — is reliably logged before the process exits,
   instead of the future being silently killed mid-flight by System/exit
   racing an in-flight connection attempt.

   Never throws, never blocks longer than `timeout-ms`, and never affects
   the backup's outcome either way (Req 12.2) — this is purely about
   making sure the warning, if any, is actually observed."
  [persist-future timeout-ms]
  (let [result (deref persist-future timeout-ms ::timed-out)]
    (when (= result ::timed-out)
      (log/warn "Async DB persist did not finish before timeout"
                {:timeout-ms timeout-ms}))))

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
     8.  db/persist-manifest!    (async, fire-and-forget — does not block
                                  or abort on failure)
     9.  upload/rclone-copy!
     10. retention/enforce-retention!
     11. delete local archive
     12. manifest/prune-local-edns!
     13. await-db-persist!       (bounded wait for step 8, so its outcome
                                  is logged before the process can exit)

   `strict-checksums?` (--strict-checksums CLI flag) is threaded to step 3
   — false (default): a per-file hash failure is logged and excluded from
   the manifest, the backup proceeds. true: the same failure aborts the
   run immediately, tagged :stage :checksum.

   Returns nil on success.
   Throws ex-info (with :stage context) on any pipeline failure."
  [config executor strict-checksums?]
  (let [dataset        (:dataset config)
        test-dataset   (:test-dataset config)
        staging-dir    (:staging-dir config)
        b2-bucket      (:b2-bucket config)
        b2-path-prefix (:b2-path-prefix config)
        b2-remote      (upload/remote-target b2-bucket b2-path-prefix)
        retention-count (:retention-count config)
        local-retention (:local-manifest-retention config)
        compression    (:compression config)
        cipher         (:encryption-cipher config)
        snap-prefix    (:snapshot-prefix config)
        pg-conn-string (get-in config [:secrets :pg-conn-string])
        ;; infisical-secrets spec, Requirement 3.3 — {} unless B2 credentials
        ;; were resolved via Infisical, in which case this carries the
        ;; RCLONE_CONFIG_B2_* env vars for every rclone subprocess call below.
        b2-rclone-env  (get-in config [:secrets :b2-rclone-env] {})
        rclone-config  (get-in config [:secrets :rclone-config])
        encryption-key (get-in config [:secrets :encryption-key])]

    ;; Validate retention count before doing any work
    (when (or (nil? retention-count) (<= retention-count 0))
      (throw (ex-info "retention-count must be a positive integer"
                      {:stage           :config
                       :retention-count retention-count})))

    ;; B2 access is required for backup to function at all — checked here,
    ;; before any real work, rather than unconditionally at
    ;; config-resolution time (see upload/ensure-b2-credentials-present!).
    (upload/ensure-b2-credentials-present! rclone-config b2-rclone-env)

    (log/info "Starting backup" {:dataset dataset})

    ;; 1. Create snapshot
    (let [now           (.toString (Instant/now))
          snap-name     (snapshot/snapshot-name dataset snap-prefix now)
          _             (timing/time-stage! :snapshot-create
                         #(snapshot/create-snapshot! executor snap-name))

          ;; 2. Compute per-file checksums from dataset mountpoint
          mountpoint    (manifest/dataset-mountpoint dataset)
          file-checksums (timing/time-stage! :file-checksums
                          #(manifest/compute-file-checksums mountpoint strict-checksums?))

          ;; 3. Check ZFS encryption status
          zfs-enc?      (snapshot/zfs-encrypted? executor dataset)

          ;; BACKUP_ENCRYPTION_KEY is only actually required when the
          ;; dataset isn't ZFS-encrypted (openssl encryption is the
          ;; fallback in that case) — checked here, now that zfs-enc? is
          ;; known, rather than unconditionally at config-resolution time.
          _             (pipeline/ensure-encryption-key-present! (not zfs-enc?) encryption-key)

          ;; 4. Build archive filename and run pipeline
          ts-compact    (pipeline/compact-timestamp now)
          slug          (pipeline/dataset-slug dataset)
          archive-fname (pipeline/archive-filename slug ts-compact zfs-enc?)
          archive-path  (str staging-dir "/" archive-fname)
          send-cmd      (snapshot/send-stream-cmd snap-name)
          pipeline-cmd  (pipeline/build-pipeline-cmd
                         send-cmd compression zfs-enc? cipher archive-path)
          _             (timing/time-stage! :send-pipeline
                         #(pipeline/run-pipeline! executor pipeline-cmd))

          ;; 5. Compute stream checksum of the archive
          stream-checksum (timing/time-stage! :stream-checksum
                           #(manifest/compute-stream-checksum executor archive-path))

          ;; 6. Build manifest
          created-at    now
          the-manifest  (manifest/build-manifest
                         dataset snap-name stream-checksum created-at
                         zfs-enc? compression archive-fname file-checksums)

          ;; 7. Write manifest EDN to staging dir
          edn-path      (timing/time-stage! :manifest-write
                         #(manifest/write-edn! the-manifest staging-dir))

          ;; 8. Persist to PostgreSQL (async, fire-and-forget — never
          ;; blocks or aborts the backup, Req 12.2). Its outcome is
          ;; awaited with a bounded timeout at step 13 below, once
          ;; everything else is done, so a failure is reliably logged
          ;; before the process can exit out from under it.
          persist-future (future
                           (try
                             (timing/time-stage! :db-persist
                              #(db/persist-manifest! pg-conn-string the-manifest))
                             (catch Exception e
                               (log/warn "Async DB persist failed:" (.getMessage e)))))]

      ;; 9. Upload archive to B2 (manifest EDN is NOT uploaded)
      (timing/time-stage! :b2-upload
       #(upload/rclone-copy! executor archive-path b2-remote b2-rclone-env))

      ;; 10. Apply retention policy
      (timing/time-stage! :retention-enforcement
       (fn []
         (let [remote-files (upload/list-remote executor b2-remote b2-rclone-env)
               sorted       (retention/sort-by-timestamp remote-files)
               ret-result   (retention/enforce-retention! executor b2-remote sorted retention-count b2-rclone-env)]
           (when (seq (:failed ret-result))
             (log/warn "Some retention deletions failed:" (:failed ret-result))))))

      ;; 11. Delete local archive (upload already succeeded) + 12. prune old
      ;; local EDN manifests — bundled as one :local-cleanup stage, both
      ;; local-disk-only operations.
      (timing/time-stage! :local-cleanup
       (fn []
         (let [f (io/file archive-path)]
           (if (.delete f)
             (log/info "Deleted local archive:" archive-path)
             (log/warn "Failed to delete local archive (non-fatal):" archive-path)))
         (manifest/prune-local-edns! staging-dir local-retention)))

      ;; 13. Bounded wait for step 8's outcome to be logged (see
      ;; await-db-persist!'s docstring) — never blocks past the timeout,
      ;; never affects this function's own return value.
      (await-db-persist! persist-future db-persist-timeout-ms)

      (log/info "Backup complete" {:snapshot snap-name :archive archive-fname})
      nil)))

;; ---------------------------------------------------------------------------
;; Connectivity tests (--db-test / --b2-test)
;; ---------------------------------------------------------------------------

(defn run-db-test!
  "Action (--db-test CLI flag). Attempts to connect to the database using
   whichever :pg-conn-string resolve-secrets resolved — env var or
   Infisical, exactly as a real backup/restore-test run would — and logs
   the result.
   Returns {:ok true} or {:ok false :error \"...\"}; never throws."
  [config]
  (let [pg-conn-string (get-in config [:secrets :pg-conn-string])
        result         (db/test-connection! pg-conn-string)]
    (if (:ok result)
      (log/info "Database connection test succeeded")
      (log/error "Database connection test failed" {:error (:error result)}))
    result))

(defn run-b2-test!
  "Action (--b2-test CLI flag). Attempts to reach the configured B2 remote
   using whichever rclone credentials resolve-secrets resolved — env var
   (RCLONE_CONFIG file) or Infisical, exactly as a real backup run would —
   and logs the result.
   Returns {:ok true} or {:ok false :error \"...\"}; never throws."
  [config executor]
  (let [b2-remote     (upload/remote-target (:b2-bucket config) (:b2-path-prefix config))
        b2-rclone-env (get-in config [:secrets :b2-rclone-env] {})
        result        (upload/test-connection! executor b2-remote b2-rclone-env)]
    (if (:ok result)
      (log/info "B2 connection test succeeded" {:remote b2-remote})
      (log/error "B2 connection test failed" {:remote b2-remote :error (:error result)}))
    result))

;; ---------------------------------------------------------------------------
;; Checksum-only diagnostic (--checksums)
;; ---------------------------------------------------------------------------

(defn throughput-files-per-sec
  "Calculation. Files-per-second throughput given a file count and elapsed
   milliseconds. Returns 0.0 when elapsed-ms is 0, guarding divide-by-zero
   for a pathologically fast or empty run."
  [file-count elapsed-ms]
  (if (zero? elapsed-ms)
    0.0
    (/ (* file-count 1000.0) elapsed-ms)))

(defn run-checksums!
  "Action (--checksums CLI flag, pipeline-timing spec Requirement 3). Runs
   ONLY file-checksum computation against the real dataset mountpoint, in
   isolation from the rest of the backup pipeline (snapshot creation,
   compression, encryption, upload) — added to diagnose the wall-clock/
   CPU-time mismatch observed on large datasets with many small files.

   Requires only :dataset from `config`. Does NOT create a snapshot, does
   NOT touch B2/rclone or the database — no :secrets key is read, and
   upload/ensure-b2-credentials-present! is never called.

   Logs file count, elapsed time, and files/sec throughput. Returns
   {:ok true :file-count N :elapsed-ms N} or {:ok false :error \"...\"};
   never throws — a --strict-checksums hash failure is caught here and
   reported as :ok false, same as any other failure mode for this
   diagnostic command (mirrors run-db-test!/run-b2-test!'s contract)."
  [config strict?]
  (let [dataset    (:dataset config)
        mountpoint (manifest/dataset-mountpoint dataset)]
    (try
      (let [{:keys [result elapsed-ms]}
            (timing/time-stage-with-result! :checksums-diagnostic
             #(manifest/compute-file-checksums mountpoint strict?))
            file-count (count result)]
        (log/info "checksum-only run complete"
                  {:dataset       dataset
                   :mountpoint    mountpoint
                   :file-count    file-count
                   :elapsed-ms    elapsed-ms
                   :files-per-sec (throughput-files-per-sec file-count elapsed-ms)})
        {:ok true :file-count file-count :elapsed-ms elapsed-ms})
      (catch Exception e
        (log/error "checksum-only run failed" {:dataset dataset :error (.getMessage e)})
        {:ok false :error (.getMessage e)}))))

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

(defn unknown-subcommand-message
  "Calculation. Builds the CLI error message for -main's default `case`
   branch — a genuinely unrecognized subcommand string (e.g. a typo).
   `subcommand` is never nil in practice here, since -main defaults a
   missing argument to \"backup\" before dispatch ever sees it, but this
   stays total/defensive rather than assuming that."
  [subcommand]
  (if (nil? subcommand)
    "No subcommand given. Valid subcommands: backup, restore-test, --db-test, --b2-test, --checksums"
    (str "Unknown subcommand: " subcommand
        ". Valid subcommands: backup, restore-test, --db-test, --b2-test, --checksums")))

(defn exit!
  "Action. Thin wrapper around System/exit.

   Exists solely as a mockable seam: on Java 21+ there is no SecurityManager
   to intercept System/exit (JEP 411 removed it), so calling it for real
   from a test would kill the whole `lein test` JVM mid-suite. Tests
   with-redefs this var instead of calling System/exit directly."
  [code]
  (System/exit code))

(defn -main
  "CLI dispatcher.

   Usage:
     strategic-backup [backup] [--strict-checksums]
     strategic-backup restore-test [--skip-verify]
     strategic-backup --db-test
     strategic-backup --b2-test
     strategic-backup --checksums [--strict-checksums]

   No subcommand at all (bare `java -jar strategic-backup.jar`) defaults to
   `backup` — the primary, most common invocation (e.g. from cron), so the
   ordinary case needs no argument at all.

   --strict-checksums (pipeline-timing spec) applies to `backup` and
   --checksums: a per-file checksum failure aborts the run instead of
   being logged and the file excluded from the manifest."
  [& args]
  (let [subcommand        (or (first args) "backup")
        skip-verify?      (some #{"--skip-verify"} (rest args))
        strict-checksums? (boolean (some #{"--strict-checksums"} (rest args)))
        executor          (shell/make-default-executor)]
    (try
      (let [cfg (-> (config/load-config nil)
                    (config/validate-config)
                    (config/resolve-secrets))]
        (case subcommand
          "backup"
          (do (run-backup! cfg executor strict-checksums?)
              (exit! 0))

          "restore-test"
          (do (run-restore-test-cmd! cfg executor (boolean skip-verify?))
              (exit! 0))

          "--db-test"
          (exit! (if (:ok (run-db-test! cfg)) 0 1))

          "--b2-test"
          (exit! (if (:ok (run-b2-test! cfg executor)) 0 1))

          "--checksums"
          (exit! (if (:ok (run-checksums! cfg strict-checksums?)) 0 1))

          (do (log/error (unknown-subcommand-message subcommand))
              (exit! 1))))
      (catch clojure.lang.ExceptionInfo e
        (log/error "Pipeline failed" {:stage   (:stage (ex-data e))
                                      :message (.getMessage e)
                                      :data    (ex-data e)})
        (exit! 1))
      (catch Exception e
        (log/error "Unexpected error:" (.getMessage e))
        (exit! 1)))))
