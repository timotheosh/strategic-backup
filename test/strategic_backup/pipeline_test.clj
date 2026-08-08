(ns strategic-backup.pipeline-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [strategic-backup.generators :as gens]
            [strategic-backup.pipeline :as pipeline]
            [strategic-backup.shell :as shell])
  (:import (java.io File)))

;; ---------------------------------------------------------------------------
;; Unit tests (task 6.2)
;; ---------------------------------------------------------------------------

(deftest archive-filename-unencrypted-extension
  (testing "produces .zfs.gz.enc extension when zfs-encrypted? is false"
    (let [result (pipeline/archive-filename "tank-syncthing" "2026-05-11T020000" false)]
      (is (str/ends-with? result ".zfs.gz.enc")))))

(deftest archive-filename-encrypted-extension
  (testing "produces .zfs.gz extension (no .enc) when zfs-encrypted? is true"
    (let [result (pipeline/archive-filename "tank-syncthing" "2026-05-11T020000" true)]
      (is (str/ends-with? result ".zfs.gz"))
      (is (not (str/ends-with? result ".zfs.gz.enc"))))))

(deftest dataset-slug-replaces-slash
  (testing "replaces / with - in dataset name"
    (is (= "tank-syncthing" (pipeline/dataset-slug "tank/syncthing")))
    (is (= "pool-a-b" (pipeline/dataset-slug "pool/a/b")))))

;; ---------------------------------------------------------------------------
;; Property 2: Archive Filename Encoding
;; Validates: Requirements 3.7, 3.8
;; ---------------------------------------------------------------------------

;; **Validates: Requirements 3.7, 3.8**
(defspec archive-filename-encoding 100
  (prop/for-all [dataset-name  gens/gen-dataset-name
                 timestamp     gens/gen-timestamp-str
                 zfs-encrypted gen/boolean]
    (let [slug     (pipeline/dataset-slug dataset-name)
          filename (pipeline/archive-filename slug timestamp zfs-encrypted)]
      (and
       ;; (a) No "/" in the resulting filename — all slashes from dataset replaced with "-"
       (not (str/includes? filename "/"))
       ;; (b) Correct suffix based on zfs-encrypted?
       (if zfs-encrypted
         ;; true  => ends with ".zfs.gz" but NOT ".zfs.gz.enc"
         (and (str/ends-with? filename ".zfs.gz")
              (not (str/ends-with? filename ".zfs.gz.enc")))
         ;; false => ends with ".zfs.gz.enc"
         (str/ends-with? filename ".zfs.gz.enc"))))))

;; ---------------------------------------------------------------------------
;; compact-timestamp
;; ---------------------------------------------------------------------------

(deftest compact-timestamp-strips-fractional-seconds-and-colons
  (testing "converts an Instant/now-style ISO string (with nanos) to compact form"
    (is (= "2026-05-11T020000"
           (pipeline/compact-timestamp "2026-05-11T02:00:00.123456789Z"))))
  (testing "works without fractional seconds too"
    (is (= "2026-05-11T020000"
           (pipeline/compact-timestamp "2026-05-11T02:00:00Z")))))

(defspec compact-timestamp-format 100
  (prop/for-all [year   (gen/choose 2000 2099)
                 month  (gen/choose 1 12)
                 day    (gen/choose 1 28)
                 hour   (gen/choose 0 23)
                 minute (gen/choose 0 59)
                 second (gen/choose 0 59)]
    (let [iso     (format "%04d-%02d-%02dT%02d:%02d:%02dZ" year month day hour minute second)
          compact (format "%04d-%02d-%02dT%02d%02d%02d" year month day hour minute second)]
      (= compact (pipeline/compact-timestamp iso)))))

;; ---------------------------------------------------------------------------
;; compress-cmd / decompress-cmd
;; ---------------------------------------------------------------------------

(deftest compress-cmd-gzip
  (is (= "gzip -c" (pipeline/compress-cmd "gzip"))))

(deftest compress-cmd-throws-for-unsupported-algorithm
  (is (thrown? clojure.lang.ExceptionInfo (pipeline/compress-cmd "bzip2"))))

(deftest decompress-cmd-gzip
  (is (= "gzip -dc" (pipeline/decompress-cmd "gzip"))))

(deftest decompress-cmd-throws-for-unsupported-algorithm
  (is (thrown? clojure.lang.ExceptionInfo (pipeline/decompress-cmd "bzip2"))))

;; ---------------------------------------------------------------------------
;; encrypt-cmd / decrypt-cmd
;; ---------------------------------------------------------------------------

(deftest encrypt-cmd-reads-key-from-env-not-literal-argument
  (testing "key is passed via -pass env:BACKUP_ENCRYPTION_KEY, never as a literal value"
    (let [cmd (pipeline/encrypt-cmd "aes-256-cbc")]
      (is (str/includes? cmd "openssl enc"))
      (is (str/includes? cmd "-aes-256-cbc"))
      (is (str/includes? cmd "-pass env:BACKUP_ENCRYPTION_KEY")))))

(deftest decrypt-cmd-uses-d-flag-and-env-pass
  (let [cmd (pipeline/decrypt-cmd "aes-256-cbc")]
    (is (str/includes? cmd "openssl enc -d"))
    (is (str/includes? cmd "-aes-256-cbc"))
    (is (str/includes? cmd "-pass env:BACKUP_ENCRYPTION_KEY"))))

;; ---------------------------------------------------------------------------
;; build-pipeline-cmd
;; ---------------------------------------------------------------------------

(deftest build-pipeline-cmd-includes-encryption-when-not-zfs-encrypted
  (testing "adds an openssl enc stage when the source dataset is not ZFS-encrypted (Req 3.3)"
    (let [cmd (pipeline/build-pipeline-cmd "zfs send -w snap" "gzip" false
                                           "aes-256-cbc" "/staging/out.zfs.gz.enc")]
      (is (str/includes? cmd "zfs send -w snap | gzip -c | openssl enc"))
      (is (str/ends-with? cmd "> /staging/out.zfs.gz.enc")))))

(deftest build-pipeline-cmd-omits-encryption-when-zfs-encrypted
  (testing "skips the openssl enc stage when the source dataset is already ZFS-encrypted (Req 3.4)"
    (let [cmd (pipeline/build-pipeline-cmd "zfs send -w snap" "gzip" true
                                           "aes-256-cbc" "/staging/out.zfs.gz")]
      (is (str/includes? cmd "zfs send -w snap | gzip -c >"))
      (is (not (str/includes? cmd "openssl"))))))

;; ---------------------------------------------------------------------------
;; extract-output-path
;; ---------------------------------------------------------------------------

(deftest extract-output-path-finds-redirect-target
  (is (= "/staging/out.zfs.gz.enc"
         (pipeline/extract-output-path "zfs send -w snap | gzip -c > /staging/out.zfs.gz.enc"))))

(deftest extract-output-path-nil-when-no-redirect
  (is (nil? (pipeline/extract-output-path "zfs send -w snap | gzip -c"))))

;; ---------------------------------------------------------------------------
;; run-pipeline!
;; ---------------------------------------------------------------------------

(deftest run-pipeline-returns-nil-on-success
  (is (nil? (pipeline/run-pipeline! (shell/make-mock-executor) "echo hi > /tmp/pipeline-test-out"))))

(deftest run-pipeline-throws-and-deletes-partial-output-on-failure
  (let [tmp      (File/createTempFile "pipeline-test" ".out")
        cmd      (str "false > " (.getAbsolutePath tmp))
        executor (shell/make-mock-executor {} {:exit 1 :out "" :err "boom" :cmd ""})]
    (try
      (pipeline/run-pipeline! executor cmd)
      (is false "expected ex-info to be thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :pipeline (:stage (ex-data e))))
        (is (not (.exists tmp)))))))
