(ns strategic-backup.restore-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [strategic-backup.restore :as restore]
            [strategic-backup.shell :as shell])
  (:import (java.io File)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- make-temp-dir []
  (let [d (File/createTempFile "restore-test" "")]
    (.delete d)
    (.mkdirs d)
    d))

(defn- delete-dir [^File dir]
  (doseq [f (reverse (file-seq dir))]
    (.delete f)))

(def ^:private sample-manifest
  {:dataset         "tank/syncthing"
   :snapshot        "tank/syncthing@backup-2026-05-11T02:00:00Z"
   :stream-checksum "sha256:abc123"
   :created-at      "2026-05-11T02:00:00Z"
   :zfs-encrypted   false
   :compression     "gzip"
   :archive-file    "tank-syncthing-2026-05-11T020000.zfs.gz.enc"
   :files           {"./a.txt" "sha256:def456"}})

(def ^:private base-config
  {:dataset           "tank/syncthing"
   :test-dataset      "tank/restore-test"
   :staging-dir       "/tmp/restore-test-staging"
   :b2-bucket         "mybucket"
   :b2-path-prefix    "zfs-backups/"
   :encryption-cipher "aes-256-cbc"
   :secrets           {:pg-conn-string nil}})

(defn- ok-executor [] (shell/make-mock-executor))

;; ---------------------------------------------------------------------------
;; resolve-manifest-mode
;; ---------------------------------------------------------------------------

(deftest resolve-manifest-mode-skip-verify-forces-mode-3
  (is (= {:mode 3 :manifest nil}
         (restore/resolve-manifest-mode sample-manifest sample-manifest true))))

(deftest resolve-manifest-mode-prefers-pg
  (is (= {:mode 1 :manifest sample-manifest}
         (restore/resolve-manifest-mode sample-manifest {:x 1} false))))

(deftest resolve-manifest-mode-falls-back-to-local-edn
  (is (= {:mode 2 :manifest sample-manifest}
         (restore/resolve-manifest-mode nil sample-manifest false))))

(deftest resolve-manifest-mode-falls-back-to-mode-3-when-neither-available
  (is (= {:mode 3 :manifest nil}
         (restore/resolve-manifest-mode nil nil false))))

;; ---------------------------------------------------------------------------
;; requires-openssl-decryption?
;; ---------------------------------------------------------------------------

(deftest requires-decryption-mode-1-2-reads-zfs-encrypted-field
  (is (true?  (restore/requires-openssl-decryption? 1 {:zfs-encrypted false} "x.zfs.gz.enc")))
  (is (false? (restore/requires-openssl-decryption? 2 {:zfs-encrypted true}  "x.zfs.gz"))))

(deftest requires-decryption-mode-3-infers-from-filename
  (is (true?  (restore/requires-openssl-decryption? 3 nil "x.zfs.gz.enc")))
  (is (false? (restore/requires-openssl-decryption? 3 nil "x.zfs.gz"))))

;; ---------------------------------------------------------------------------
;; needs-zfs-key? (ZFS encryption passphrase — only needed when verification
;; is about to run against a ZFS-natively-encrypted dataset)
;; ---------------------------------------------------------------------------

(deftest needs-zfs-key-true-when-verifying-a-zfs-encrypted-dataset
  (is (true? (restore/needs-zfs-key? 1 false)))
  (is (true? (restore/needs-zfs-key? 2 false))))

(deftest needs-zfs-key-false-when-openssl-encrypted-instead
  (is (false? (restore/needs-zfs-key? 1 true)))
  (is (false? (restore/needs-zfs-key? 2 true))))

(deftest needs-zfs-key-false-in-mode-3-regardless-of-decrypt
  (testing "mode 3 skips verification entirely, so the key is never needed"
    (is (false? (restore/needs-zfs-key? 3 false)))
    (is (false? (restore/needs-zfs-key? 3 true)))))

;; ---------------------------------------------------------------------------
;; ensure-zfs-key-loaded!
;; ---------------------------------------------------------------------------

(deftest ensure-zfs-key-loaded-no-op-when-not-needed
  (testing "does not call snapshot/load-key! when needs-key? is false"
    (let [called (atom false)]
      (with-redefs [strategic-backup.snapshot/load-key! (fn [_ _ _] (reset! called true))]
        (restore/ensure-zfs-key-loaded! (ok-executor) "tank/restore-test" false nil)
        (is (false? @called))))))

(deftest ensure-zfs-key-loaded-throws-when-needed-and-passphrase-missing
  (testing "throws ex-info :stage :restore immediately, without ever calling
            snapshot/load-key!, when the passphrase isn't configured"
    (let [called (atom false)]
      (with-redefs [strategic-backup.snapshot/load-key! (fn [_ _ _] (reset! called true))]
        (try
          (restore/ensure-zfs-key-loaded! (ok-executor) "tank/restore-test" true nil)
          (is false "expected ex-info to be thrown")
          (catch clojure.lang.ExceptionInfo e
            (is (= :restore (:stage (ex-data e))))))
        (is (false? @called))))))

(deftest ensure-zfs-key-loaded-calls-load-key-when-needed-and-passphrase-present
  (testing "calls snapshot/load-key! with the exact dataset and passphrase"
    (let [captured (atom nil)]
      (with-redefs [strategic-backup.snapshot/load-key!
                    (fn [_ dataset passphrase] (reset! captured [dataset passphrase]) dataset)]
        (restore/ensure-zfs-key-loaded! (ok-executor) "tank/restore-test" true "s3kr3t")
        (is (= ["tank/restore-test" "s3kr3t"] @captured))))))

;; ---------------------------------------------------------------------------
;; latest-archive
;; ---------------------------------------------------------------------------

(deftest latest-archive-picks-newest
  (is (= "a-2026-01-01T000000.zfs.gz"
         (restore/latest-archive ["a-2024-01-01T000000.zfs.gz"
                                   "a-2026-01-01T000000.zfs.gz"
                                   "a-2025-01-01T000000.zfs.gz"]))))

(deftest latest-archive-nil-when-empty
  (is (nil? (restore/latest-archive []))))

;; ---------------------------------------------------------------------------
;; infer-compression
;; ---------------------------------------------------------------------------

(deftest infer-compression-gzip-with-and-without-enc
  (is (= "gzip" (restore/infer-compression "x-2026-01-01T000000.zfs.gz")))
  (is (= "gzip" (restore/infer-compression "x-2026-01-01T000000.zfs.gz.enc"))))

(deftest infer-compression-throws-for-unrecognized-extension
  (is (thrown? clojure.lang.ExceptionInfo (restore/infer-compression "x.tar"))))

;; ---------------------------------------------------------------------------
;; run-restore-pipeline!
;; ---------------------------------------------------------------------------

(deftest run-restore-pipeline-returns-nil-on-success
  (is (nil? (restore/run-restore-pipeline! (ok-executor) "cat x | gzip -dc | zfs receive y"))))

(deftest run-restore-pipeline-throws-on-failure
  (let [fail-executor (shell/make-mock-executor {} {:exit 1 :out "" :err "boom" :cmd ""})]
    (try
      (restore/run-restore-pipeline! fail-executor "cat x | zfs receive y")
      (is false "expected ex-info to be thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :restore (:stage (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; run-restore-test! — orchestration (mode selection, cleanup-always)
;; ---------------------------------------------------------------------------

(deftest run-restore-test-mode-1-happy-path
  (testing "mode 1 (pg manifest) runs the full pipeline and verifies successfully"
    (let [destroyed (atom false)
          staging   (make-temp-dir)
          config    (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest         (fn [_ _] sample-manifest)
                      strategic-backup.manifest/latest-local-edn        (fn [_] nil)
                      strategic-backup.upload/download-archive!         (fn [_ _ _ dir & _env]
                                                                          (spit (str dir "/" (:archive-file sample-manifest)) "data")
                                                                          nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc123")
                      strategic-backup.restore/run-restore-pipeline!    (fn [_ _] nil)
                      strategic-backup.verify/verify-file-checksums!    (fn [_ _ _] {:ok true :matched 1 :mismatched [] :missing []})
                      strategic-backup.snapshot/destroy-dataset!        (fn [_ _] (reset! destroyed true) nil)]
          (restore/run-restore-test! config (ok-executor) false))
        (is (true? @destroyed))
        (is (not (.exists (io/file (.getAbsolutePath staging) (:archive-file sample-manifest)))))
        (finally (delete-dir staging))))))

(deftest run-restore-test-destroys-stale-test-dataset-before-receiving
  (testing "destroys the test dataset defensively BEFORE zfs receive, in addition to the always-cleanup at the end
            (self-heals a dataset left behind by an abnormally-terminated previous run, since zfs receive
            fails outright if the destination already exists)"
    (let [call-log (atom [])
          staging  (make-temp-dir)
          config   (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest         (fn [_ _] sample-manifest)
                      strategic-backup.manifest/latest-local-edn        (fn [_] nil)
                      strategic-backup.upload/download-archive!         (fn [_ _ _ dir & _env]
                                                                          (swap! call-log conj :download)
                                                                          (spit (str dir "/" (:archive-file sample-manifest)) "data")
                                                                          nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc123")
                      strategic-backup.restore/run-restore-pipeline!    (fn [_ _] (swap! call-log conj :pipeline) nil)
                      strategic-backup.verify/verify-file-checksums!    (fn [_ _ _] {:ok true :matched 1 :mismatched [] :missing []})
                      strategic-backup.snapshot/destroy-dataset!        (fn [_ _] (swap! call-log conj :destroy) nil)]
          (restore/run-restore-test! config (ok-executor) false))
        ;; destroy must appear before the pipeline (pre-cleanup) AND after it (Req 8.6 cleanup)
        (is (= [:destroy :download :pipeline :destroy] @call-log))
        (finally (delete-dir staging))))))

(deftest run-restore-test-mode-3-skips-verification
  (testing "mode 3 (skip-verify) downloads the latest archive and skips checksum verification"
    (let [verify-called (atom false)
          staging       (make-temp-dir)
          config        (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest      (fn [_ _] nil)
                      strategic-backup.manifest/latest-local-edn     (fn [_] nil)
                      strategic-backup.upload/list-remote            (fn [_ _ & _env] ["x-2026-01-01T000000.zfs.gz"])
                      strategic-backup.upload/download-archive!      (fn [_ _ _ _ & _env] nil)
                      strategic-backup.restore/run-restore-pipeline! (fn [_ _] nil)
                      strategic-backup.verify/verify-file-checksums! (fn [_ _ _] (reset! verify-called true) {:ok true})
                      strategic-backup.snapshot/destroy-dataset!     (fn [_ _] nil)]
          (restore/run-restore-test! config (ok-executor) true))
        (is (false? @verify-called))
        (finally (delete-dir staging))))))

(deftest run-restore-test-passes-b2-rclone-env-to-upload-calls
  (testing "the resolved :b2-rclone-env (infisical-secrets spec, Requirement 3.3) is threaded to list-remote and download-archive!"
    (let [captured-envs (atom [])
          staging       (make-temp-dir)
          b2-env        {"RCLONE_CONFIG_B2_TYPE" "b2"}
          config        (-> base-config
                            (assoc :staging-dir (.getAbsolutePath staging))
                            (assoc-in [:secrets :b2-rclone-env] b2-env))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest      (fn [_ _] nil)
                      strategic-backup.manifest/latest-local-edn     (fn [_] nil)
                      strategic-backup.upload/list-remote
                      (fn [_ _ env] (swap! captured-envs conj env) ["x-2026-01-01T000000.zfs.gz"])
                      strategic-backup.upload/download-archive!
                      (fn [_ _ _ _ env] (swap! captured-envs conj env) nil)
                      strategic-backup.restore/run-restore-pipeline! (fn [_ _] nil)
                      strategic-backup.snapshot/destroy-dataset!     (fn [_ _] nil)]
          (restore/run-restore-test! config (ok-executor) true))
        (is (= 2 (count @captured-envs)))
        (is (every? #(= b2-env %) @captured-envs))
        (finally (delete-dir staging))))))

(deftest run-restore-test-cleans-up-on-stream-checksum-mismatch
  (testing "destroys the test dataset and deletes the archive even when stream checksum verification fails"
    (let [destroyed (atom false)
          staging   (make-temp-dir)
          config    (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest         (fn [_ _] sample-manifest)
                      strategic-backup.manifest/latest-local-edn        (fn [_] nil)
                      strategic-backup.upload/download-archive!         (fn [_ _ _ dir & _env]
                                                                          (spit (str dir "/" (:archive-file sample-manifest)) "data")
                                                                          nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:WRONG")
                      strategic-backup.snapshot/destroy-dataset!        (fn [_ _] (reset! destroyed true) nil)]
          (try
            (restore/run-restore-test! config (ok-executor) false)
            (is false "expected ex-info to be thrown")
            (catch clojure.lang.ExceptionInfo e
              (is (= :verify (:stage (ex-data e)))))))
        (is (true? @destroyed))
        (is (not (.exists (io/file (.getAbsolutePath staging) (:archive-file sample-manifest)))))
        (finally (delete-dir staging))))))

(deftest run-restore-test-cleans-up-on-file-checksum-mismatch
  (testing "destroys the test dataset even when per-file verification fails"
    (let [destroyed (atom false)
          staging   (make-temp-dir)
          config    (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest         (fn [_ _] sample-manifest)
                      strategic-backup.manifest/latest-local-edn        (fn [_] nil)
                      strategic-backup.upload/download-archive!         (fn [_ _ _ dir & _env]
                                                                          (spit (str dir "/" (:archive-file sample-manifest)) "data")
                                                                          nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc123")
                      strategic-backup.restore/run-restore-pipeline!    (fn [_ _] nil)
                      strategic-backup.verify/verify-file-checksums!    (fn [_ _ _] {:ok false :mismatched [{:path "./a.txt"}] :missing []})
                      strategic-backup.snapshot/destroy-dataset!        (fn [_ _] (reset! destroyed true) nil)]
          (try
            (restore/run-restore-test! config (ok-executor) false)
            (is false "expected ex-info to be thrown")
            (catch clojure.lang.ExceptionInfo e
              (is (= :verify (:stage (ex-data e)))))))
        (is (true? @destroyed))
        (finally (delete-dir staging))))))

(deftest run-restore-test-cleans-up-on-pipeline-failure
  (testing "destroys the test dataset even when the zfs receive pipeline fails"
    (let [destroyed (atom false)
          staging   (make-temp-dir)
          config    (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest         (fn [_ _] sample-manifest)
                      strategic-backup.manifest/latest-local-edn        (fn [_] nil)
                      strategic-backup.upload/download-archive!         (fn [_ _ _ dir & _env]
                                                                          (spit (str dir "/" (:archive-file sample-manifest)) "data")
                                                                          nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc123")
                      strategic-backup.restore/run-restore-pipeline!
                      (fn [_ _] (throw (ex-info "zfs receive failed" {:stage :restore})))
                      strategic-backup.snapshot/destroy-dataset!        (fn [_ _] (reset! destroyed true) nil)]
          (try
            (restore/run-restore-test! config (ok-executor) false)
            (is false "expected ex-info to be thrown")
            (catch clojure.lang.ExceptionInfo e
              (is (= :restore (:stage (ex-data e)))))))
        (is (true? @destroyed))
        (finally (delete-dir staging))))))

;; ---------------------------------------------------------------------------
;; run-restore-test! — ZFS encryption passphrase (zfs load-key) integration
;; ---------------------------------------------------------------------------

(def ^:private encrypted-manifest
  (assoc sample-manifest :zfs-encrypted true :archive-file "tank-syncthing-2026-05-11T020000.zfs.gz"))

(deftest run-restore-test-loads-zfs-key-before-verification-for-encrypted-dataset
  (testing "when the dataset was ZFS-encrypted at backup time, the key is loaded
            (via snapshot/load-key!) after zfs receive and before file-checksum
            verification"
    (let [call-log (atom [])
          staging  (make-temp-dir)
          config   (-> base-config
                       (assoc :staging-dir (.getAbsolutePath staging))
                       (assoc-in [:secrets :zfs-encryption-passphrase] "s3kr3t"))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest         (fn [_ _] encrypted-manifest)
                      strategic-backup.manifest/latest-local-edn        (fn [_] nil)
                      strategic-backup.upload/download-archive!         (fn [_ _ _ dir & _env]
                                                                          (spit (str dir "/" (:archive-file encrypted-manifest)) "data")
                                                                          nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc123")
                      strategic-backup.restore/run-restore-pipeline!    (fn [_ _] (swap! call-log conj :pipeline) nil)
                      strategic-backup.snapshot/load-key!               (fn [_ dataset _] (swap! call-log conj :load-key) dataset)
                      strategic-backup.verify/verify-file-checksums!    (fn [_ _ _] (swap! call-log conj :verify) {:ok true :matched 1 :mismatched [] :missing []})
                      strategic-backup.snapshot/destroy-dataset!        (fn [_ _] (swap! call-log conj :destroy) nil)]
          (restore/run-restore-test! config (ok-executor) false))
        (is (= [:destroy :pipeline :load-key :verify :destroy] @call-log))
        (finally (delete-dir staging))))))

(deftest run-restore-test-aborts-loudly-when-encrypted-and-passphrase-missing
  (testing "throws before verification and still cleans up when the dataset is
            ZFS-encrypted but no passphrase is configured"
    (let [destroyed     (atom false)
          verify-called (atom false)
          staging       (make-temp-dir)
          config        (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest         (fn [_ _] encrypted-manifest)
                      strategic-backup.manifest/latest-local-edn        (fn [_] nil)
                      strategic-backup.upload/download-archive!         (fn [_ _ _ dir & _env]
                                                                          (spit (str dir "/" (:archive-file encrypted-manifest)) "data")
                                                                          nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc123")
                      strategic-backup.restore/run-restore-pipeline!    (fn [_ _] nil)
                      strategic-backup.verify/verify-file-checksums!    (fn [_ _ _] (reset! verify-called true) {:ok true})
                      strategic-backup.snapshot/destroy-dataset!        (fn [_ _] (reset! destroyed true) nil)]
          (try
            (restore/run-restore-test! config (ok-executor) false)
            (is false "expected ex-info to be thrown")
            (catch clojure.lang.ExceptionInfo e
              (is (= :restore (:stage (ex-data e)))))))
        (is (false? @verify-called))
        (is (true? @destroyed))
        (finally (delete-dir staging))))))

(deftest run-restore-test-skips-key-loading-for-openssl-encrypted-dataset
  (testing "no zfs load-key attempt for a dataset that used openssl encryption
            instead of native ZFS encryption, even with no passphrase configured"
    (let [load-key-called (atom false)
          staging         (make-temp-dir)
          config          (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.db/fetch-latest-manifest         (fn [_ _] sample-manifest)
                      strategic-backup.manifest/latest-local-edn        (fn [_] nil)
                      strategic-backup.upload/download-archive!         (fn [_ _ _ dir & _env]
                                                                          (spit (str dir "/" (:archive-file sample-manifest)) "data")
                                                                          nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc123")
                      strategic-backup.restore/run-restore-pipeline!    (fn [_ _] nil)
                      strategic-backup.snapshot/load-key!               (fn [_ _ _] (reset! load-key-called true))
                      strategic-backup.verify/verify-file-checksums!    (fn [_ _ _] {:ok true :matched 1 :mismatched [] :missing []})
                      strategic-backup.snapshot/destroy-dataset!        (fn [_ _] nil)]
          (restore/run-restore-test! config (ok-executor) false))
        (is (false? @load-key-called))
        (finally (delete-dir staging))))))

(deftest run-restore-test-throws-when-no-archive-available
  (testing "throws ex-info :stage :restore when there is nothing to restore"
    (with-redefs [strategic-backup.db/fetch-latest-manifest  (fn [_ _] nil)
                  strategic-backup.manifest/latest-local-edn (fn [_] nil)
                  strategic-backup.upload/list-remote        (fn [_ _ & _env] [])]
      (try
        (restore/run-restore-test! base-config (ok-executor) false)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :restore (:stage (ex-data e)))))))))
