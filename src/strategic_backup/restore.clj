(ns strategic-backup.restore
  "Restore test orchestration (Requirements 7, 8)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [strategic-backup.db :as db]
            [strategic-backup.manifest :as manifest]
            [strategic-backup.pipeline :as pipeline]
            [strategic-backup.retention :as retention]
            [strategic-backup.shell :as shell]
            [strategic-backup.snapshot :as snapshot]
            [strategic-backup.upload :as upload]
            [strategic-backup.verify :as verify]
            [taoensso.timbre :as log]))

(defn resolve-manifest-mode
  "Calculation — the three-tier fallback decision (Req 7.1). Given the
   already-fetched candidate manifests (or nil, if unavailable) and the
   explicit skip-verify flag, decides which restore mode to run in:
     1 — a PostgreSQL manifest is available
     2 — no PostgreSQL manifest, but a local EDN manifest is available
     3 — neither is available, or skip-verify was explicitly requested"
  [pg-manifest local-edn-manifest skip-verify?]
  (cond
    skip-verify?               {:mode 3 :manifest nil}
    (some? pg-manifest)        {:mode 1 :manifest pg-manifest}
    (some? local-edn-manifest) {:mode 2 :manifest local-edn-manifest}
    :else                      {:mode 3 :manifest nil}))

(defn requires-openssl-decryption?
  "Calculation (Req 7.7-7.9). Mode 1/2: read the manifest's :zfs-encrypted
   field — decryption is needed unless the source dataset was already
   ZFS-encrypted at backup time. Mode 3 (no manifest available): infer
   from the archive filename — a \".enc\" suffix means openssl encryption
   was applied at backup time."
  [mode manifest archive-filename]
  (if (= mode 3)
    (str/ends-with? archive-filename ".enc")
    (not (:zfs-encrypted manifest))))

(defn latest-archive
  "Calculation (Req 7.3, Mode 3). The most recent archive filename among
   `remote-filenames`, by embedded timestamp, or nil if there are none."
  [remote-filenames]
  (last (retention/sort-by-timestamp remote-filenames)))

(defn infer-compression
  "Calculation (Req 7.10, Mode 3). Infers the compression algorithm from
   an archive filename's extension. Currently only \".zfs.gz[.enc]\"
   (gzip) is ever produced by this system's backup pipeline."
  [archive-filename]
  (if (str/includes? archive-filename ".zfs.gz")
    "gzip"
    (throw (ex-info "Cannot infer compression from archive filename"
                    {:stage :restore :archive-filename archive-filename}))))

(defn run-restore-pipeline!
  "Action. Executes the restore shell pipeline (cat archive | [decrypt] |
   decompress | zfs receive) via the executor.
   Returns nil on success; throws ex-info with :stage :restore on
   non-zero exit (Req 7.12)."
  [executor pipeline-cmd]
  (let [result (shell/run-pipeline executor pipeline-cmd)]
    (when (not= 0 (:exit result))
      (throw (ex-info "Restore pipeline failed"
                      {:stage :restore
                       :cmd   (:cmd result)
                       :exit  (:exit result)
                       :err   (:err result)})))
    nil))

(defn run-restore-test!
  "Orchestrates the weekly automated restore test (Req 7, 8).

   Order: resolve manifest mode (Req 7.1) -> determine + download the
   archive (Req 7.2-7.4) -> verify stream checksum, mode 1/2 only
   (Req 7.5-7.6) -> decrypt/decompress/zfs receive (Req 7.7-7.12) ->
   verify per-file checksums, mode 1/2 only, or log skipped in mode 3
   (Req 8.1-8.5) -> always destroy the Test_Dataset and delete the
   downloaded archive (Req 8.6-8.7), regardless of outcome.

   Returns nil on success. Throws ex-info (with :stage context) on any
   verification or pipeline failure — after cleanup has already run."
  [config executor skip-verify?]
  (let [dataset        (:dataset config)
        test-dataset   (:test-dataset config)
        staging-dir    (:staging-dir config)
        b2-remote      (upload/remote-target (:b2-bucket config) (:b2-path-prefix config))
        cipher         (:encryption-cipher config)
        pg-conn-string (get-in config [:secrets :pg-conn-string])
        ;; infisical-secrets spec, Requirement 3.3 — see core.clj's run-backup!
        b2-rclone-env  (get-in config [:secrets :b2-rclone-env] {})

        pg-manifest    (db/fetch-latest-manifest pg-conn-string dataset)
        local-manifest (manifest/latest-local-edn staging-dir)
        {:keys [mode manifest]} (resolve-manifest-mode pg-manifest local-manifest skip-verify?)]

    (log/info "Starting restore test" {:mode mode})

    (let [archive-fname (if (= mode 3)
                          (latest-archive (upload/list-remote executor b2-remote b2-rclone-env))
                          (:archive-file manifest))]
      (when (nil? archive-fname)
        (throw (ex-info "No archive available to restore"
                        {:stage :restore :mode mode})))

      (let [archive-path (str staging-dir "/" archive-fname)
            compression  (if (= mode 3) (infer-compression archive-fname) (:compression manifest))
            decrypt?     (requires-openssl-decryption? mode manifest archive-fname)]
        (try
          (upload/download-archive! executor b2-remote archive-fname staging-dir b2-rclone-env)

          ;; Req 7.5/7.6 — verify the stream checksum before touching the
          ;; archive at all; a mismatch aborts before decrypt/decompress.
          (when (not= mode 3)
            (let [actual (manifest/compute-stream-checksum executor archive-path)]
              (when-not (verify/verify-stream-checksum? (:stream-checksum manifest) actual)
                (log/error "Stream checksum mismatch"
                          {:expected (:stream-checksum manifest) :actual actual})
                (throw (ex-info "Archive stream checksum verification failed"
                                {:stage    :verify
                                 :expected (:stream-checksum manifest)
                                 :actual   actual})))))

          ;; Req 7.7-7.12 — decrypt (if needed), decompress, zfs receive
          (run-restore-pipeline!
           executor
           (pipeline/build-restore-pipeline-cmd archive-path compression decrypt? cipher test-dataset))

          ;; Req 8.1-8.5 (mode 1/2) or Req 8.2 (mode 3 — skipped, logged)
          (if (= mode 3)
            (log/warn "Restore verification skipped — Mode 3, no manifest available"
                      {:archive archive-fname})
            (let [test-mountpoint (manifest/dataset-mountpoint test-dataset)
                  result          (verify/verify-file-checksums! executor test-mountpoint manifest)]
              (when-not (:ok result)
                (throw (ex-info "Restore file checksum verification failed"
                                {:stage      :verify
                                 :mismatched (:mismatched result)
                                 :missing    (:missing result)})))))

          (log/info "Restore test complete"
                    {:mode mode :snapshot (:snapshot manifest) :archive archive-fname})
          nil

          (finally
            ;; Req 8.6 — always destroy the Test_Dataset, pass or fail.
            (snapshot/destroy-dataset! executor test-dataset)
            ;; Req 8.7 — always delete the downloaded archive; local EDN
            ;; manifest files are never touched by this cleanup step.
            (let [f (io/file archive-path)]
              (when (.exists f) (.delete f)))))))))
