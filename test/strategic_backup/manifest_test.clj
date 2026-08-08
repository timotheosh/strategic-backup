(ns strategic-backup.manifest-test
  "Unit and property tests for the manifest namespace.

  **Validates: Requirements 2.4**"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [strategic-backup.generators :as gen]
            [strategic-backup.manifest :as manifest]
            [strategic-backup.shell :as shell])
  (:import (java.io File)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- make-temp-dir []
  (let [d (File/createTempFile "manifest-test" "")]
    (.delete d)
    (.mkdirs d)
    d))

(defn- delete-dir [^File dir]
  (doseq [f (reverse (file-seq dir))]
    (.delete f)))

(defn- openssl-executor
  "A MockShellExecutor that answers any `openssl dgst -sha256` call with a
   fixed digest, mirroring the real output format."
  []
  (shell/make-mock-executor
   {} {:exit 0
       :out  "SHA256(f)= aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n"
       :err  ""
       :cmd  ""}))

;; ---------------------------------------------------------------------------
;; dataset-mountpoint
;; ---------------------------------------------------------------------------

(deftest dataset-mountpoint-prefixes-slash
  (testing "prepends / to the dataset name (existing behavior, preserved as-is)"
    (is (= "/tank/syncthing" (manifest/dataset-mountpoint "tank/syncthing")))))

;; ---------------------------------------------------------------------------
;; parse-openssl-checksum
;; ---------------------------------------------------------------------------

(deftest parse-openssl-checksum-extracts-hex-digest
  (testing "parses \"SHA256(file)= <hex>\" into \"sha256:<hex>\""
    (is (= "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
           (manifest/parse-openssl-checksum
            "SHA256(/tank/syncthing/foo.txt)= aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n")))))

(deftest parse-openssl-checksum-nil-when-unparseable
  (is (nil? (manifest/parse-openssl-checksum "not a checksum line"))))

;; ---------------------------------------------------------------------------
;; relativize-path
;; ---------------------------------------------------------------------------

(deftest relativize-path-strips-mountpoint-prefix
  (testing "mountpoint without trailing slash"
    (is (= "./Documents/foo.pdf"
           (manifest/relativize-path "/tank/syncthing" "/tank/syncthing/Documents/foo.pdf"))))
  (testing "mountpoint with trailing slash"
    (is (= "./foo.pdf"
           (manifest/relativize-path "/tank/syncthing/" "/tank/syncthing/foo.pdf")))))

;; ---------------------------------------------------------------------------
;; manifest-filename
;; ---------------------------------------------------------------------------

(deftest manifest-filename-appends-suffix
  (is (= "tank-syncthing-2026-05-11T020000.zfs.gz.enc.manifest.edn"
         (manifest/manifest-filename "tank-syncthing-2026-05-11T020000.zfs.gz.enc"))))

;; ---------------------------------------------------------------------------
;; build-manifest
;; ---------------------------------------------------------------------------

(deftest build-manifest-assembles-all-required-fields
  (let [m (manifest/build-manifest
           "tank/syncthing"
           "tank/syncthing@backup-2026-05-11T02:00:00Z"
           "sha256:abc123"
           "2026-05-11T02:00:00Z"
           false
           "gzip"
           "tank-syncthing-2026-05-11T020000.zfs.gz.enc"
           {"./foo.txt" "sha256:def456"})]
    (is (= #{:dataset :snapshot :stream-checksum :created-at :zfs-encrypted
             :compression :archive-file :files}
           (set (keys m))))
    (is (= "tank/syncthing" (:dataset m)))
    (is (false? (:zfs-encrypted m)))
    (is (= {"./foo.txt" "sha256:def456"} (:files m)))))

;; ---------------------------------------------------------------------------
;; compute-file-checksums
;; ---------------------------------------------------------------------------

(deftest compute-file-checksums-walks-and-checksums-files
  (let [dir (make-temp-dir)]
    (try
      (spit (io/file dir "a.txt") "hello")
      (.mkdirs (io/file dir "sub"))
      (spit (io/file dir "sub" "b.txt") "world")
      (let [result (manifest/compute-file-checksums (openssl-executor) (.getAbsolutePath dir))]
        (is (= 2 (count result)))
        (is (every? #(str/starts-with? % "./") (keys result)))
        (is (every? #(str/starts-with? % "sha256:") (vals result))))
      (finally (delete-dir dir)))))

;; ---------------------------------------------------------------------------
;; compute-stream-checksum
;; ---------------------------------------------------------------------------

(deftest compute-stream-checksum-returns-parsed-digest
  (is (= "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         (manifest/compute-stream-checksum (openssl-executor) "/staging/archive.zfs.gz.enc"))))

;; ---------------------------------------------------------------------------
;; write-edn!
;; ---------------------------------------------------------------------------

(deftest write-edn-writes-file-named-after-archive
  (let [dir (make-temp-dir)
        m   {:dataset "tank/syncthing" :archive-file "tank-syncthing-2026-05-11T020000.zfs.gz.enc"}]
    (try
      (let [path (manifest/write-edn! m (.getAbsolutePath dir))]
        (is (str/ends-with? path "tank-syncthing-2026-05-11T020000.zfs.gz.enc.manifest.edn"))
        (is (.exists (io/file path)))
        (is (= m (edn/read-string (slurp path)))))
      (finally (delete-dir dir)))))

;; ---------------------------------------------------------------------------
;; prune-local-edns!
;; ---------------------------------------------------------------------------

(deftest prune-local-edns-keeps-newest-n
  (let [dir   (make-temp-dir)
        older "tank-syncthing-2024-01-01T000000.zfs.gz.manifest.edn"
        newer "tank-syncthing-2026-01-01T000000.zfs.gz.manifest.edn"]
    (try
      (spit (io/file dir older) "{}")
      (spit (io/file dir newer) "{}")
      (let [deleted-count (manifest/prune-local-edns! (.getAbsolutePath dir) 1)]
        (is (= 1 deleted-count))
        (is (not (.exists (io/file dir older))))
        (is (.exists (io/file dir newer))))
      (finally (delete-dir dir)))))

(deftest prune-local-edns-deletes-nothing-within-retention
  (let [dir (make-temp-dir)]
    (try
      (spit (io/file dir "a-2026-01-01T000000.zfs.gz.manifest.edn") "{}")
      (is (= 0 (manifest/prune-local-edns! (.getAbsolutePath dir) 5)))
      (finally (delete-dir dir)))))

;; ---------------------------------------------------------------------------
;; Property 5: Manifest EDN Round-Trip
;;
;; For any valid manifest map with all required top-level fields, serializing
;; to EDN and parsing back SHALL produce a map equal under Clojure's `=`.
;;
;; **Validates: Requirements 2.4**
;; ---------------------------------------------------------------------------

(defspec manifest-edn-round-trip
  100
  (prop/for-all [manifest gen/gen-manifest]
    (= manifest (edn/read-string (pr-str manifest)))))
