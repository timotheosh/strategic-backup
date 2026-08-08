(ns strategic-backup.snapshot-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [strategic-backup.generators :as gen]
            [strategic-backup.shell :as shell]
            [strategic-backup.snapshot :as snapshot]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- ok-executor
  "A MockShellExecutor that always succeeds (exit 0)."
  []
  (shell/make-mock-executor))

(defn- fail-executor
  "A MockShellExecutor that always fails (exit 1)."
  []
  (shell/make-mock-executor {} {:exit 1 :out "" :err "simulated error" :cmd ""}))

;; ---------------------------------------------------------------------------
;; snapshot-name (pure function)
;; ---------------------------------------------------------------------------

(deftest snapshot-name-format
  (testing "produces <dataset>@<prefix>-<timestamp>"
    (is (= "tank/syncthing@backup-2026-05-11T02:00:00Z"
           (snapshot/snapshot-name "tank/syncthing" "backup" "2026-05-11T02:00:00Z"))))
  (testing "dataset and prefix appear verbatim"
    (let [result (snapshot/snapshot-name "pool/data" "nightly" "2025-01-01T000000Z")]
      (is (str/starts-with? result "pool/data@"))
      (is (str/includes? result "@nightly-"))))
  (testing "separator @ appears between dataset and prefix"
    (let [result (snapshot/snapshot-name "a/b" "p" "ts")]
      (is (= "a/b@p-ts" result)))))

;; ---------------------------------------------------------------------------
;; send-stream-cmd (pure function)
;; ---------------------------------------------------------------------------

(deftest send-stream-cmd-always-includes-w-flag
  (testing "always includes the -w flag for raw send"
    (let [cmd (snapshot/send-stream-cmd "tank/syncthing@backup-2026-05-11T020000")]
      (is (str/includes? cmd "-w"))))
  (testing "includes the snapshot name in the command"
    (let [snap "tank/data@backup-2026-01-01T000000"
          cmd  (snapshot/send-stream-cmd snap)]
      (is (str/includes? cmd snap))))
  (testing "starts with zfs send"
    (is (str/starts-with?
         (snapshot/send-stream-cmd "tank/s@snap")
         "zfs send"))))

;; ---------------------------------------------------------------------------
;; create-snapshot! (uses executor)
;; ---------------------------------------------------------------------------

(deftest create-snapshot-invokes-zfs-snapshot-with-exact-name
  (testing "calls zfs snapshot with the exact snapshot name"
    (let [snap     "tank/syncthing@backup-2026-05-11T020000"
          ;; Mock responds to the exact constructed command string
          executor (shell/make-mock-executor
                    {(str "zfs snapshot " snap)
                     {:exit 0 :out "" :err "" :cmd (str "zfs snapshot " snap)}})]
      (is (= snap (snapshot/create-snapshot! executor snap))))))

(deftest create-snapshot-throws-ex-info-on-non-zero-exit
  (testing "throws ex-info with :stage :snapshot on non-zero exit"
    (try
      (snapshot/create-snapshot! (fail-executor) "tank/data@snap")
      (is false "expected ex-info to be thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :snapshot (:stage (ex-data e))))))))

(deftest create-snapshot-returns-snapshot-name-on-success
  (testing "returns the snapshot name on success"
    (let [snap "tank/data@backup-ts"]
      (is (= snap (snapshot/create-snapshot! (ok-executor) snap))))))

;; ---------------------------------------------------------------------------
;; encrypted-value? (pure calculation extracted from zfs-encrypted?)
;; ---------------------------------------------------------------------------

(deftest encrypted-value-false-for-off
  (testing "\"off\" (with or without trailing newline) is not encrypted"
    (is (false? (snapshot/encrypted-value? "off")))
    (is (false? (snapshot/encrypted-value? "off\n")))))

(deftest encrypted-value-true-for-any-other-value
  (testing "any non-off value is encrypted"
    (is (true? (snapshot/encrypted-value? "aes-256-gcm")))
    (is (true? (snapshot/encrypted-value? "on\n")))))

;; ---------------------------------------------------------------------------
;; zfs-encrypted? (uses executor)
;; ---------------------------------------------------------------------------

(deftest zfs-encrypted-returns-false-for-off
  (testing "returns false when encryption value is 'off'"
    (let [executor (shell/make-mock-executor {} {:exit 0 :out "off\n" :err "" :cmd ""})]
      (is (false? (snapshot/zfs-encrypted? executor "tank/data"))))))

(deftest zfs-encrypted-returns-true-for-non-off-values
  (testing "returns true when encryption value is 'aes-256-gcm'"
    (let [executor (shell/make-mock-executor {} {:exit 0 :out "aes-256-gcm\n" :err "" :cmd ""})]
      (is (true? (snapshot/zfs-encrypted? executor "tank/data")))))
  (testing "returns true when encryption value is 'aes-128-ccm'"
    (let [executor (shell/make-mock-executor {} {:exit 0 :out "aes-128-ccm\n" :err "" :cmd ""})]
      (is (true? (snapshot/zfs-encrypted? executor "tank/data")))))
  (testing "returns true for any non-off string"
    (let [executor (shell/make-mock-executor {} {:exit 0 :out "on\n" :err "" :cmd ""})]
      (is (true? (snapshot/zfs-encrypted? executor "tank/data"))))))

;; ---------------------------------------------------------------------------
;; receive-stream! (uses executor)
;; ---------------------------------------------------------------------------

(deftest receive-stream-throws-ex-info-on-non-zero-exit
  (testing "throws ex-info on non-zero exit from zfs receive"
    (try
      (snapshot/receive-stream! (fail-executor) "tank/restore-test")
      (is false "expected ex-info to be thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :receive (:stage (ex-data e))))))))

(deftest receive-stream-returns-dataset-on-success
  (testing "returns the test-dataset name on success"
    (is (= "tank/restore-test"
           (snapshot/receive-stream! (ok-executor) "tank/restore-test")))))

;; ---------------------------------------------------------------------------
;; destroy-dataset! (uses executor)
;; ---------------------------------------------------------------------------

(deftest destroy-dataset-does-not-throw-on-failure
  (testing "does not throw even when zfs destroy fails"
    (is (nil? (snapshot/destroy-dataset! (fail-executor) "tank/restore-test")))))

(deftest destroy-dataset-returns-nil-on-success
  (testing "returns nil on success"
    (is (nil? (snapshot/destroy-dataset! (ok-executor) "tank/restore-test")))))

;; ---------------------------------------------------------------------------
;; Property 1: Snapshot Name Format
;; Validates: Requirements 1.2
;; ---------------------------------------------------------------------------

;; **Validates: Requirements 1.2**
(defspec snapshot-name-format-property 100
  (prop/for-all [dataset  gen/gen-dataset-name
                 prefix   gen/gen-snapshot-prefix
                 ts       gen/gen-iso8601-str]
    (= (snapshot/snapshot-name dataset prefix ts)
       (str dataset "@" prefix "-" ts))))
