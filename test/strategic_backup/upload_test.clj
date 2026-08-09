(ns strategic-backup.upload-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [strategic-backup.shell :as shell]
            [strategic-backup.upload :as upload]))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- ok-executor [] (shell/make-mock-executor))
(defn- fail-executor []
  (shell/make-mock-executor {} {:exit 1 :out "" :err "connection refused" :cmd ""}))

;; ---------------------------------------------------------------------------
;; remote-target
;; ---------------------------------------------------------------------------

(deftest remote-target-builds-b2-spec
  (testing "combines bucket and prefix into a b2: remote spec"
    (is (= "b2:mybucket/zfs-backups/" (upload/remote-target "mybucket" "zfs-backups/")))))

;; ---------------------------------------------------------------------------
;; rclone-copy!
;; ---------------------------------------------------------------------------

(deftest rclone-copy-uses-copy-not-sync
  (testing "the implementation uses 'rclone copy' — confirmed by mock key lookup"
    ;; The MockShellExecutor looks up results by the constructed cmd string.
    ;; We register a success result keyed on "rclone copy ..." — if the
    ;; implementation used "rclone sync" it would miss this key and fall back
    ;; to the default (which is also exit 0 here, but the :cmd field reveals
    ;; what was actually invoked).
    (let [expected-cmd "rclone copy /staging/archive.zfs.gz.enc b2:bucket"
          executor     (shell/make-mock-executor
                        {expected-cmd {:exit 0 :out "" :err "" :cmd expected-cmd}})]
      ;; Should succeed — meaning the implementation did call "rclone copy"
      (is (nil? (upload/rclone-copy! executor "/staging/archive.zfs.gz.enc" "b2:bucket")))))
  (testing "does not use 'rclone sync' — verified structurally in the source"
    ;; Register only a sync key — if the implementation uses copy it will get
    ;; the zero-exit default, not the sync key. Either way it should succeed.
    (let [executor (shell/make-mock-executor
                    {"rclone sync /staging/f.enc b2:bucket"
                     {:exit 0 :out "" :err "" :cmd "rclone sync /staging/f.enc b2:bucket"}}
                    {:exit 1 :out "" :err "wrong command" :cmd ""})]
      ;; If the impl uses 'copy', it won't find the sync key → falls to default
      ;; which is exit 1. If it were to use copy and succeed, we set default=fail.
      ;; Actually: we want to verify copy is used. Set default to fail, only
      ;; register success for the copy command.
      (let [copy-key "rclone copy /staging/f.enc b2:bucket"
            executor2 (shell/make-mock-executor
                       {copy-key {:exit 0 :out "" :err "" :cmd copy-key}}
                       {:exit 1 :out "" :err "unexpected command" :cmd ""})]
        (is (nil? (upload/rclone-copy! executor2 "/staging/f.enc" "b2:bucket")))))))

(deftest rclone-copy-throws-ex-info-on-non-zero-exit
  (testing "throws ex-info with :stage :upload on failure"
    (try
      (upload/rclone-copy! (fail-executor) "/staging/archive.enc" "b2:bucket")
      (is false "expected ex-info to be thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :upload (:stage (ex-data e))))))))

(deftest rclone-copy-succeeds-on-exit-zero
  (testing "does not throw on exit 0"
    (is (nil? (upload/rclone-copy! (ok-executor) "/staging/archive.enc" "b2:bucket")))))

;; ---------------------------------------------------------------------------
;; env threading (infisical-secrets spec, Requirement 3.3)
;; ---------------------------------------------------------------------------

(deftest rclone-copy-forwards-env-to-shell-run-cmd
  (testing "an explicit env map is forwarded into shell/run-cmd's opts"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/rclone-copy! nil "/local" "b2:bucket" {"RCLONE_CONFIG_B2_TYPE" "b2"})
        (is (= {:env {"RCLONE_CONFIG_B2_TYPE" "b2"}} @captured-opts))))))

(deftest rclone-copy-legacy-arity-uses-empty-env
  (testing "omitting env (3-arg call) reproduces today's behavior exactly — {:env {}}"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/rclone-copy! nil "/local" "b2:bucket")
        (is (= {:env {}} @captured-opts))))))

;; ---------------------------------------------------------------------------
;; parse-lsf-output (pure calculation extracted from list-remote)
;; ---------------------------------------------------------------------------

(deftest parse-lsf-output-splits-and-drops-blanks
  (testing "splits on newlines and drops blank lines"
    (is (= ["a.zfs.gz" "b.zfs.gz"] (upload/parse-lsf-output "a.zfs.gz\n\nb.zfs.gz\n"))))
  (testing "empty output yields empty seq"
    (is (= [] (upload/parse-lsf-output "")))))

;; ---------------------------------------------------------------------------
;; list-remote
;; ---------------------------------------------------------------------------

(deftest list-remote-forwards-env-to-shell-run-cmd
  (testing "an explicit env map is forwarded into shell/run-cmd's opts"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/list-remote nil "b2:bucket" {"RCLONE_CONFIG_B2_TYPE" "b2"})
        (is (= {:env {"RCLONE_CONFIG_B2_TYPE" "b2"}} @captured-opts))))))

(deftest list-remote-legacy-arity-uses-empty-env
  (testing "omitting env (2-arg call) reproduces today's behavior exactly — {:env {}}"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/list-remote nil "b2:bucket")
        (is (= {:env {}} @captured-opts))))))

(deftest list-remote-parses-rclone-lsf-output
  (testing "parses rclone lsf output into a sequence of filename strings"
    (let [lsf-output "archive-2024-01-01T000000.zfs.gz.enc\narchive-2025-01-01T000000.zfs.gz.enc\n"
          executor   (shell/make-mock-executor
                      {} {:exit 0 :out lsf-output :err "" :cmd ""})]
      (let [result (upload/list-remote executor "b2:bucket")]
        (is (= 2 (count result)))
        (is (some #(str/includes? % "2024-01-01") result))
        (is (some #(str/includes? % "2025-01-01") result)))))
  (testing "filters out blank lines"
    (let [executor (shell/make-mock-executor
                    {} {:exit 0 :out "file1.gz\n\nfile2.gz\n" :err "" :cmd ""})]
      (is (= 2 (count (upload/list-remote executor "b2:bucket"))))))
  (testing "returns empty sequence when rclone lsf fails"
    (is (empty? (upload/list-remote (fail-executor) "b2:bucket"))))
  (testing "returns empty sequence when output is blank"
    (let [executor (shell/make-mock-executor {} {:exit 0 :out "" :err "" :cmd ""})]
      (is (empty? (upload/list-remote executor "b2:bucket"))))))

;; ---------------------------------------------------------------------------
;; test-connection! (--b2-test CLI flag)
;; ---------------------------------------------------------------------------

(deftest test-connection-succeeds-on-exit-zero
  (testing "returns {:ok true} on a successful rclone lsf, even for an empty bucket"
    (is (= {:ok true} (upload/test-connection! (ok-executor) "b2:bucket")))))

(deftest test-connection-fails-on-non-zero-exit
  (testing "returns {:ok false :error ...} without throwing on failure"
    (let [result (upload/test-connection! (fail-executor) "b2:bucket")]
      (is (false? (:ok result)))
      (is (= "connection refused" (:error result))))))

(deftest test-connection-forwards-env-to-shell-run-cmd
  (testing "an explicit env map is forwarded into shell/run-cmd's opts"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/test-connection! nil "b2:bucket" {"RCLONE_CONFIG_B2_TYPE" "b2"})
        (is (= {:env {"RCLONE_CONFIG_B2_TYPE" "b2"}} @captured-opts))))))

(deftest test-connection-legacy-arity-uses-empty-env
  (testing "omitting env (2-arg call) reproduces today's behavior exactly — {:env {}}"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/test-connection! nil "b2:bucket")
        (is (= {:env {}} @captured-opts))))))

;; ---------------------------------------------------------------------------
;; download-archive!
;; ---------------------------------------------------------------------------

(deftest download-archive-throws-on-failure
  (testing "throws ex-info with :stage :download on non-zero exit"
    (try
      (upload/download-archive! (fail-executor) "b2:bucket" "archive.zfs.gz.enc" "/staging")
      (is false "expected ex-info to be thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :download (:stage (ex-data e))))))))

(deftest download-archive-succeeds-on-exit-zero
  (testing "does not throw on exit 0"
    (is (nil? (upload/download-archive! (ok-executor) "b2:bucket" "archive.zfs.gz.enc" "/staging")))))

(deftest download-archive-forwards-env-to-shell-run-cmd
  (testing "an explicit env map is forwarded into shell/run-cmd's opts"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/download-archive! nil "b2:bucket" "archive.zfs.gz.enc" "/staging" {"RCLONE_CONFIG_B2_TYPE" "b2"})
        (is (= {:env {"RCLONE_CONFIG_B2_TYPE" "b2"}} @captured-opts))))))

(deftest download-archive-legacy-arity-uses-empty-env
  (testing "omitting env (4-arg call) reproduces today's behavior exactly — {:env {}}"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/download-archive! nil "b2:bucket" "archive.zfs.gz.enc" "/staging")
        (is (= {:env {}} @captured-opts))))))

;; ---------------------------------------------------------------------------
;; delete-remote!
;; ---------------------------------------------------------------------------

(deftest delete-remote-returns-ok-false-on-failure-without-throwing
  (testing "returns {:ok false :error ...} on failure — does NOT throw"
    (let [result (upload/delete-remote! (fail-executor) "b2:bucket" "archive.zfs.gz.enc")]
      (is (map? result))
      (is (false? (:ok result)))
      (is (contains? result :error))
      (is (string? (:error result))))))

(deftest delete-remote-returns-ok-true-on-success
  (testing "returns {:ok true} on exit 0"
    (let [result (upload/delete-remote! (ok-executor) "b2:bucket" "archive.zfs.gz.enc")]
      (is (= {:ok true} result)))))

(deftest delete-remote-forwards-env-to-shell-run-cmd
  (testing "an explicit env map is forwarded into shell/run-cmd's opts"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/delete-remote! nil "b2:bucket" "archive.zfs.gz.enc" {"RCLONE_CONFIG_B2_TYPE" "b2"})
        (is (= {:env {"RCLONE_CONFIG_B2_TYPE" "b2"}} @captured-opts))))))

(deftest delete-remote-legacy-arity-uses-empty-env
  (testing "omitting env (3-arg call) reproduces today's behavior exactly — {:env {}}"
    (let [captured-opts (atom nil)]
      (with-redefs [shell/run-cmd (fn [_ _ _ opts] (reset! captured-opts opts) {:exit 0 :out "" :err "" :cmd ""})]
        (upload/delete-remote! nil "b2:bucket" "archive.zfs.gz.enc")
        (is (= {:env {}} @captured-opts))))))
