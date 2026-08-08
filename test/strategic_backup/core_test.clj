(ns strategic-backup.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [strategic-backup.shell :as shell]
            [strategic-backup.core :as core])
  (:import (java.io File)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- make-temp-dir []
  (let [d (File/createTempFile "core-test" "")]
    (.delete d)
    (.mkdirs d)
    d))

(defn- delete-dir [^File dir]
  (doseq [f (reverse (file-seq dir))]
    (.delete f)))

(def ^:private base-config
  {:dataset                  "tank/syncthing"
   :test-dataset             "tank/restore-test"
   :staging-dir              "/tmp/core-test-staging"
   :b2-bucket                "mybucket"
   :b2-path-prefix           "zfs-backups/"
   :retention-count          7
   :local-manifest-retention 14
   :compression              "gzip"
   :encryption-cipher        "aes-256-cbc"
   :snapshot-prefix          "backup"
   :secrets                  {:encryption-key  "test-key"
                               :rclone-config   "/etc/rclone.conf"
                               :pg-conn-string  nil}})

;; All-success executor
(defn- ok-executor []
  (shell/make-mock-executor
   {} {:exit 0
       :out  "SHA256(f)= aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"
       :err  ""
       :cmd  ""}))

;; ---------------------------------------------------------------------------
;; run-backup! — pipeline structural tests (using with-redefs)
;; ---------------------------------------------------------------------------

(deftest backup-uses-rclone-copy-not-sync
  (testing "run-backup! uses rclone copy for upload, never rclone sync"
    (let [upload-calls (atom [])
          staging      (make-temp-dir)
          config       (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.snapshot/create-snapshot!       (fn [_ _] nil)
                      strategic-backup.snapshot/zfs-encrypted?          (fn [_ _] false)
                      strategic-backup.manifest/compute-file-checksums  (fn [_ _] {})
                      strategic-backup.pipeline/run-pipeline!           (fn [_ _] nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc")
                      strategic-backup.manifest/write-edn!              (fn [m _]
                                                                          (str (.getAbsolutePath staging)
                                                                               "/" (:archive-file m)))
                      strategic-backup.db/persist-manifest!             (fn [_ _] {:ok true})
                      strategic-backup.upload/rclone-copy!
                      (fn [_ local-path remote _env]
                        (swap! upload-calls conj {:local local-path :remote remote}))
                      strategic-backup.upload/list-remote               (fn [_ _ _] [])
                      strategic-backup.retention/enforce-retention!     (fn [_ _ _ _ _] {:deleted [] :failed []})
                      strategic-backup.manifest/prune-local-edns!       (fn [_ _] 0)]
          (core/run-backup! config (ok-executor)))
        (is (= 1 (count @upload-calls)))
        ;; The remote path should not be a sync call — we verify structurally
        ;; that rclone-copy! was called (the implementation hardcodes "rclone copy")
        (is (str/includes? (:remote (first @upload-calls)) "b2:mybucket"))
        (finally (delete-dir staging))))))

(deftest backup-does-not-upload-manifest-edn
  (testing "the EDN manifest file is NOT passed to rclone-copy!"
    (let [upload-args  (atom nil)
          staging      (make-temp-dir)
          config       (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.snapshot/create-snapshot!       (fn [_ _] nil)
                      strategic-backup.snapshot/zfs-encrypted?          (fn [_ _] false)
                      strategic-backup.manifest/compute-file-checksums  (fn [_ _] {})
                      strategic-backup.pipeline/run-pipeline!           (fn [_ _] nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc")
                      strategic-backup.manifest/write-edn!              (fn [m _]
                                                                          (str (.getAbsolutePath staging) "/"
                                                                               (:archive-file m) ".manifest.edn"))
                      strategic-backup.db/persist-manifest!             (fn [_ _] {:ok true})
                      strategic-backup.upload/rclone-copy!
                      (fn [_ local-path _remote _env] (reset! upload-args local-path))
                      strategic-backup.upload/list-remote               (fn [_ _ _] [])
                      strategic-backup.retention/enforce-retention!     (fn [_ _ _ _ _] {:deleted [] :failed []})
                      strategic-backup.manifest/prune-local-edns!       (fn [_ _] 0)]
          (core/run-backup! config (ok-executor)))
        ;; The uploaded file must NOT be the EDN manifest
        (is (not (str/ends-with? (or @upload-args "") ".manifest.edn")))
        (finally (delete-dir staging))))))

(deftest backup-db-failure-does-not-abort
  (testing "a PostgreSQL failure during persist-manifest! does not abort the backup run"
    (let [backup-completed (atom false)
          staging          (make-temp-dir)
          config           (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.snapshot/create-snapshot!       (fn [_ _] nil)
                      strategic-backup.snapshot/zfs-encrypted?          (fn [_ _] false)
                      strategic-backup.manifest/compute-file-checksums  (fn [_ _] {})
                      strategic-backup.pipeline/run-pipeline!           (fn [_ _] nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc")
                      strategic-backup.manifest/write-edn!              (fn [m _]
                                                                          (str (.getAbsolutePath staging) "/"
                                                                               (:archive-file m)))
                      ;; DB fails
                      strategic-backup.db/persist-manifest!
                      (fn [_ _] (throw (ex-info "DB unavailable" {:stage :db})))
                      strategic-backup.upload/rclone-copy!              (fn [_ _ _ _] nil)
                      strategic-backup.upload/list-remote               (fn [_ _ _] [])
                      strategic-backup.retention/enforce-retention!     (fn [_ _ _ _ _] {:deleted [] :failed []})
                      strategic-backup.manifest/prune-local-edns!
                      (fn [_ _] (reset! backup-completed true) 0)]
          (core/run-backup! config (ok-executor)))
        ;; If we reach here, backup completed despite DB failure
        (is (true? @backup-completed))
        (finally (delete-dir staging))))))

(deftest backup-deletes-local-archive-after-upload
  (testing "the local archive file is deleted after a successful upload"
    (let [staging (make-temp-dir)
          config  (assoc base-config :staging-dir (.getAbsolutePath staging))
          ;; We'll create a fake archive file and check it gets deleted
          fake-archive (atom nil)]
      (try
        (with-redefs [strategic-backup.snapshot/create-snapshot!       (fn [_ _] nil)
                      strategic-backup.snapshot/zfs-encrypted?          (fn [_ _] false)
                      strategic-backup.manifest/compute-file-checksums  (fn [_ _] {})
                      strategic-backup.pipeline/run-pipeline!
                      (fn [_ cmd]
                        ;; Create the archive file so delete has something to remove
                        (let [out-path (second (re-find #">\s*(\S+)" cmd))]
                          (when out-path
                            (reset! fake-archive out-path)
                            (spit out-path "fake-data"))))
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc")
                      strategic-backup.manifest/write-edn!              (fn [m _]
                                                                          (str (.getAbsolutePath staging) "/"
                                                                               (:archive-file m) ".manifest.edn"))
                      strategic-backup.db/persist-manifest!             (fn [_ _] {:ok true})
                      strategic-backup.upload/rclone-copy!              (fn [_ _ _ _] nil)
                      strategic-backup.upload/list-remote               (fn [_ _ _] [])
                      strategic-backup.retention/enforce-retention!     (fn [_ _ _ _ _] {:deleted [] :failed []})
                      strategic-backup.manifest/prune-local-edns!       (fn [_ _] 0)]
          (core/run-backup! config (ok-executor)))
        ;; Archive file must be deleted
        (when @fake-archive
          (is (not (.exists (io/file @fake-archive)))))
        (finally (delete-dir staging))))))

(deftest backup-does-not-delete-local-edn-manifest
  (testing "the local EDN manifest file is NOT deleted after upload"
    (let [staging      (make-temp-dir)
          config       (assoc base-config :staging-dir (.getAbsolutePath staging))
          edn-path-ref (atom nil)]
      (try
        (with-redefs [strategic-backup.snapshot/create-snapshot!       (fn [_ _] nil)
                      strategic-backup.snapshot/zfs-encrypted?          (fn [_ _] false)
                      strategic-backup.manifest/compute-file-checksums  (fn [_ _] {})
                      strategic-backup.pipeline/run-pipeline!           (fn [_ _] nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc")
                      strategic-backup.manifest/write-edn!
                      (fn [m dir]
                        (let [path (str dir "/" (:archive-file m) ".manifest.edn")]
                          (reset! edn-path-ref path)
                          (spit path (pr-str m))
                          path))
                      strategic-backup.db/persist-manifest!             (fn [_ _] {:ok true})
                      strategic-backup.upload/rclone-copy!              (fn [_ _ _ _] nil)
                      strategic-backup.upload/list-remote               (fn [_ _ _] [])
                      strategic-backup.retention/enforce-retention!     (fn [_ _ _ _ _] {:deleted [] :failed []})
                      strategic-backup.manifest/prune-local-edns!       (fn [_ _] 0)]
          (core/run-backup! config (ok-executor)))
        ;; EDN file must still exist
        (when @edn-path-ref
          (is (.exists (io/file @edn-path-ref))))
        (finally (delete-dir staging))))))

(deftest backup-aborts-on-non-positive-retention-count
  (testing "a non-positive retention count aborts the backup run before upload"
    (let [upload-called (atom false)
          staging       (make-temp-dir)
          config        (assoc base-config
                               :staging-dir (.getAbsolutePath staging)
                               :retention-count 0)]
      (try
        (with-redefs [strategic-backup.upload/rclone-copy!
                      (fn [_ _ _ _] (reset! upload-called true))]
          (try
            (core/run-backup! config (ok-executor))
            (is false "expected ex-info to be thrown")
            (catch clojure.lang.ExceptionInfo e
              (is (= :config (:stage (ex-data e))))
              (is (false? @upload-called)))))
        (finally (delete-dir staging))))))

(deftest backup-passes-b2-rclone-env-to-upload-and-retention-calls
  (testing "the resolved :b2-rclone-env (infisical-secrets spec, Requirement 3.3) is threaded through to every upload/retention call"
    (let [captured-envs (atom [])
          staging       (make-temp-dir)
          b2-env        {"RCLONE_CONFIG_B2_TYPE"    "b2"
                         "RCLONE_CONFIG_B2_ACCOUNT" "key-id"
                         "RCLONE_CONFIG_B2_KEY"     "app-key"}
          config        (-> base-config
                            (assoc :staging-dir (.getAbsolutePath staging))
                            (assoc-in [:secrets :b2-rclone-env] b2-env))]
      (try
        (with-redefs [strategic-backup.snapshot/create-snapshot!       (fn [_ _] nil)
                      strategic-backup.snapshot/zfs-encrypted?          (fn [_ _] false)
                      strategic-backup.manifest/compute-file-checksums  (fn [_ _] {})
                      strategic-backup.pipeline/run-pipeline!           (fn [_ _] nil)
                      strategic-backup.manifest/compute-stream-checksum (fn [_ _] "sha256:abc")
                      strategic-backup.manifest/write-edn!              (fn [m _]
                                                                          (str (.getAbsolutePath staging) "/" (:archive-file m)))
                      strategic-backup.db/persist-manifest!             (fn [_ _] {:ok true})
                      strategic-backup.upload/rclone-copy!
                      (fn [_ _ _ env] (swap! captured-envs conj env))
                      strategic-backup.upload/list-remote
                      (fn [_ _ env] (swap! captured-envs conj env) [])
                      strategic-backup.retention/enforce-retention!
                      (fn [_ _ _ _ env] (swap! captured-envs conj env) {:deleted [] :failed []})
                      strategic-backup.manifest/prune-local-edns!       (fn [_ _] 0)]
          (core/run-backup! config (ok-executor)))
        (is (= 3 (count @captured-envs)))
        (is (every? #(= b2-env %) @captured-envs))
        (finally (delete-dir staging))))))

;; ---------------------------------------------------------------------------
;; -main exit code tests
;; ---------------------------------------------------------------------------

(deftest main-exits-zero-on-successful-backup
  (testing "-main exits with code 0 on a successful backup run"
    (let [exit-code (atom nil)
          staging   (make-temp-dir)
          config    (assoc base-config :staging-dir (.getAbsolutePath staging))]
      (try
        (with-redefs [strategic-backup.config/load-config     (fn [_] config)
                      strategic-backup.config/validate-config (fn [c] c)
                      strategic-backup.config/resolve-secrets (fn [c] c)
                      core/run-backup!                        (fn [_ _] nil)
                      core/exit!                              (fn [code] (reset! exit-code code))]
          (core/-main "backup"))
        (is (= 0 @exit-code))
        (finally (delete-dir staging))))))

(deftest main-exits-one-on-pipeline-failure
  (testing "-main exits with code 1 when any pipeline stage throws ex-info"
    (let [exit-code (atom nil)
          staging   (make-temp-dir)]
      (try
        (with-redefs [strategic-backup.config/load-config     (fn [_] (assoc base-config :staging-dir (.getAbsolutePath staging)))
                      strategic-backup.config/validate-config (fn [c] c)
                      strategic-backup.config/resolve-secrets (fn [c] c)
                      strategic-backup.snapshot/create-snapshot!
                      (fn [_ _] (throw (ex-info "snapshot failed" {:stage :snapshot})))
                      core/exit! (fn [code] (reset! exit-code code))]
          (core/-main "backup"))
        (is (= 1 @exit-code))
        (finally (delete-dir staging))))))
