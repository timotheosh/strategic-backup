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
;; format-hex
;; ---------------------------------------------------------------------------

(deftest format-hex-converts-bytes-to-lowercase-hex
  (testing "known byte values convert to their known lowercase hex string"
    (is (= "00ff7f" (manifest/format-hex (byte-array [0 -1 127]))))
    (is (= "" (manifest/format-hex (byte-array 0))))))

;; ---------------------------------------------------------------------------
;; hash-file — real temp files, independently-verified digests
;; (via `printf '%s' "<content>" | shasum -a 256`)
;; ---------------------------------------------------------------------------

(deftest hash-file-computes-known-sha256-of-known-content
  (let [dir (make-temp-dir)]
    (try
      (let [hello (io/file dir "hello.txt")
            empty (io/file dir "empty.txt")
            world (io/file dir "world.txt")]
        (spit hello "hello")
        (spit empty "")
        (spit world "world")
        (is (= "sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
               (manifest/hash-file hello)))
        (is (= "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
               (manifest/hash-file empty)))
        (is (= "sha256:486ea46224d1bb4fb680f34f7c9ad96a8f24ec88be73ea8e5a6c65260e9cb8a7"
               (manifest/hash-file world))))
      (finally (delete-dir dir)))))

(deftest hash-file-throws-on-read-failure
  (testing "propagates the I/O failure rather than catching it — compute-file-checksums
            owns the catch/exclude-or-propagate decision, not hash-file itself"
    (is (thrown? java.io.IOException
                 (manifest/hash-file (io/file "/nonexistent/definitely/not/a/real/path.txt"))))))

;; ---------------------------------------------------------------------------
;; compute-file-checksums
;; ---------------------------------------------------------------------------

(deftest compute-file-checksums-walks-and-checksums-files
  (let [dir (make-temp-dir)]
    (try
      (spit (io/file dir "a.txt") "hello")
      (.mkdirs (io/file dir "sub"))
      (spit (io/file dir "sub" "b.txt") "world")
      (let [result (manifest/compute-file-checksums (.getAbsolutePath dir) false 1)]
        (is (= 2 (count result)))
        (is (= "sha256:2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
               (get result "./a.txt")))
        (is (= "sha256:486ea46224d1bb4fb680f34f7c9ad96a8f24ec88be73ea8e5a6c65260e9cb8a7"
               (get result "./sub/b.txt"))))
      (finally (delete-dir dir)))))

(deftest compute-file-checksums-empty-dir-returns-empty-map
  (let [dir (make-temp-dir)]
    (try
      (is (= {} (manifest/compute-file-checksums (.getAbsolutePath dir) false 1)))
      (finally (delete-dir dir)))))

(deftest compute-file-checksums-non-strict-excludes-failed-file-not-nil
  (testing "a file that fails to hash is entirely absent from the result map —
            never present with a nil/bogus checksum (the fix for the silent
            manifest-corruption bug)"
    (let [dir            (make-temp-dir)
          real-hash-file manifest/hash-file]
      (try
        (spit (io/file dir "good.txt") "hello")
        (spit (io/file dir "bad.txt") "world")
        (with-redefs [manifest/hash-file
                      (fn [f]
                        (if (= "bad.txt" (.getName ^java.io.File f))
                          (throw (java.io.IOException. "simulated failure"))
                          (real-hash-file f)))]
          (let [result (manifest/compute-file-checksums (.getAbsolutePath dir) false 1)]
            (is (= 1 (count result)))
            (is (contains? result "./good.txt"))
            (is (not (contains? result "./bad.txt")))
            (is (not (contains? (set (vals result)) nil)))))
        (finally (delete-dir dir))))))

(deftest compute-file-checksums-strict-throws-on-first-failure
  (testing "strict? true propagates the failure as ex-info :stage :checksum,
            instead of excluding the file and continuing"
    (let [dir            (make-temp-dir)
          real-hash-file manifest/hash-file]
      (try
        (spit (io/file dir "bad.txt") "world")
        (with-redefs [manifest/hash-file
                      (fn [f]
                        (if (= "bad.txt" (.getName ^java.io.File f))
                          (throw (java.io.IOException. "simulated failure"))
                          (real-hash-file f)))]
          (try
            (manifest/compute-file-checksums (.getAbsolutePath dir) true 1)
            (is false "expected ex-info to be thrown")
            (catch clojure.lang.ExceptionInfo e
              (is (= :checksum (:stage (ex-data e)))))))
        (finally (delete-dir dir))))))

;; ---------------------------------------------------------------------------
;; compute-file-checksums — concurrency (pipeline-timing spec, Requirement 4)
;; Property 4: Concurrency Never Changes The Result
;; ---------------------------------------------------------------------------

(deftest compute-file-checksums-same-result-at-concurrency-1-and-greater
  (testing "identical result map regardless of concurrency, for a set of
            successfully-hashed files"
    (let [dir (make-temp-dir)]
      (try
        (doseq [n (range 10)]
          (spit (io/file dir (str "f" n ".txt")) (str "content-" n)))
        (let [path        (.getAbsolutePath dir)
              sequential  (manifest/compute-file-checksums path false 1)
              concurrent  (manifest/compute-file-checksums path false 4)
              max-concur  (manifest/compute-file-checksums path false 100)]
          (is (= 10 (count sequential)))
          (is (= sequential concurrent))
          (is (= sequential max-concur)))
        (finally (delete-dir dir))))))

(deftest compute-file-checksums-non-strict-excludes-failed-file-under-concurrency
  (testing "the non-strict exclusion behavior (Property 1) holds identically when
            concurrency > 1 — the failing file is still absent, not present-as-nil,
            and every other file is still hashed correctly"
    (let [dir            (make-temp-dir)
          real-hash-file manifest/hash-file]
      (try
        (doseq [n (range 8)]
          (spit (io/file dir (str "f" n ".txt")) (str "content-" n)))
        (spit (io/file dir "bad.txt") "world")
        (with-redefs [manifest/hash-file
                      (fn [f]
                        (if (= "bad.txt" (.getName ^java.io.File f))
                          (throw (java.io.IOException. "simulated failure"))
                          (real-hash-file f)))]
          (let [result (manifest/compute-file-checksums (.getAbsolutePath dir) false 4)]
            (is (= 8 (count result)))
            (is (not (contains? result "./bad.txt")))
            (is (not (contains? (set (vals result)) nil)))))
        (finally (delete-dir dir))))))

(deftest compute-file-checksums-strict-throws-plain-ex-info-under-concurrency
  (testing "a strict-mode failure under concurrency > 1 still surfaces as a plain
            ex-info :stage :checksum — never wrapped in
            java.util.concurrent.ExecutionException — because deref on a Clojure
            future unwraps a worker thread's exception to its original type"
    (let [dir            (make-temp-dir)
          real-hash-file manifest/hash-file]
      (try
        (doseq [n (range 8)]
          (spit (io/file dir (str "f" n ".txt")) (str "content-" n)))
        (spit (io/file dir "bad.txt") "world")
        (with-redefs [manifest/hash-file
                      (fn [f]
                        (if (= "bad.txt" (.getName ^java.io.File f))
                          (throw (java.io.IOException. "simulated failure"))
                          (real-hash-file f)))]
          (try
            (manifest/compute-file-checksums (.getAbsolutePath dir) true 4)
            (is false "expected ex-info to be thrown")
            (catch java.util.concurrent.ExecutionException _e
              (is false "exception leaked as ExecutionException — deref did not unwrap it"))
            (catch clojure.lang.ExceptionInfo e
              (is (= :checksum (:stage (ex-data e)))))))
        (finally (delete-dir dir))))))

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
