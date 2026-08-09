(ns strategic-backup.shell-test
  (:require [clojure.test :refer [deftest is testing]]
            [strategic-backup.shell :as shell]))

;; ---------------------------------------------------------------------------
;; MockShellExecutor tests
;; ---------------------------------------------------------------------------

(deftest mock-executor-run-cmd-returns-configured-result
  (testing "run-cmd returns the pre-configured result for a known command"
    (let [executor (shell/make-mock-executor
                    {"echo hello" {:exit 0 :out "hello\n" :err "" :cmd "echo hello"}})]
      (let [result (shell/run-cmd executor "echo" ["hello"] {})]
        (is (= 0 (:exit result)))
        (is (= "hello\n" (:out result)))
        (is (= "echo hello" (:cmd result)))))))

(deftest mock-executor-run-cmd-includes-cmd-in-result
  (testing "run-cmd always sets :cmd to the constructed command string"
    (let [executor (shell/make-mock-executor)]
      (let [result (shell/run-cmd executor "zfs" ["snapshot" "tank/data@backup"] {})]
        (is (= "zfs snapshot tank/data@backup" (:cmd result)))))))

(deftest mock-executor-run-cmd-uses-default-result-for-unknown-cmd
  (testing "run-cmd returns default result when command is not in responses map"
    (let [executor (shell/make-mock-executor {} {:exit 1 :out "" :err "not found" :cmd ""})]
      (let [result (shell/run-cmd executor "unknown" ["arg"] {})]
        (is (= 1 (:exit result)))
        (is (= "not found" (:err result)))))))

(deftest mock-executor-run-cmd-default-exit-zero-when-no-default
  (testing "run-cmd returns exit 0 when command not found and no default-result set"
    (let [executor (shell/make-mock-executor)]
      (let [result (shell/run-cmd executor "anything" [] {})]
        (is (= 0 (:exit result)))))))

(deftest mock-executor-run-pipeline-returns-configured-result
  (testing "run-pipeline returns the pre-configured result for a known pipeline string"
    (let [pipeline "zfs send -w tank/data@snap | gzip -c > /tmp/out.gz"
          executor (shell/make-mock-executor
                    {pipeline {:exit 0 :out "" :err "" :cmd pipeline}})]
      (let [result (shell/run-pipeline executor pipeline)]
        (is (= 0 (:exit result)))
        (is (= pipeline (:cmd result)))))))

(deftest mock-executor-run-pipeline-sets-cmd-field
  (testing "run-pipeline always sets :cmd to the shell string"
    (let [executor (shell/make-mock-executor)
          pipeline "echo hello | cat"]
      (let [result (shell/run-pipeline executor pipeline)]
        (is (= pipeline (:cmd result)))))))

(deftest mock-executor-run-pipeline-uses-default-result
  (testing "run-pipeline returns default-result for unknown pipeline strings"
    (let [executor (shell/make-mock-executor {} {:exit 2 :out "" :err "pipeline failed" :cmd ""})]
      (let [result (shell/run-pipeline executor "unknown pipeline")]
        (is (= 2 (:exit result)))
        (is (= "pipeline failed" (:err result)))))))

(deftest mock-executor-result-map-has-required-keys
  (testing "every result map from MockShellExecutor contains :exit :out :err :cmd"
    (let [executor (shell/make-mock-executor)]
      (let [cmd-result      (shell/run-cmd executor "ls" ["-la"] {})
            pipeline-result (shell/run-pipeline executor "ls -la")]
        (is (contains? cmd-result :exit))
        (is (contains? cmd-result :out))
        (is (contains? cmd-result :err))
        (is (contains? cmd-result :cmd))
        (is (contains? pipeline-result :exit))
        (is (contains? pipeline-result :out))
        (is (contains? pipeline-result :err))
        (is (contains? pipeline-result :cmd))))))

;; ---------------------------------------------------------------------------
;; DefaultShellExecutor smoke test (runs a real command)
;; ---------------------------------------------------------------------------

(deftest default-executor-run-cmd-returns-result-map
  (testing "DefaultShellExecutor run-cmd returns a result map with required keys"
    (let [executor (shell/make-default-executor)
          result   (shell/run-cmd executor "echo" ["hello"] {})]
      (is (= 0 (:exit result)))
      (is (contains? result :out))
      (is (contains? result :err))
      (is (= "echo hello" (:cmd result))))))

(deftest default-executor-run-pipeline-executes-via-sh
  (testing "DefaultShellExecutor run-pipeline executes a shell string and returns result map"
    (let [executor (shell/make-default-executor)
          pipeline "echo pipeline-test"
          result   (shell/run-pipeline executor pipeline)]
      (is (= 0 (:exit result)))
      (is (= pipeline (:cmd result))))))

(deftest default-executor-run-cmd-forwards-in-as-subprocess-stdin
  (testing ":in in opts is piped to the subprocess's stdin — needed so callers
            (e.g. zfs load-key) can supply a secret without ever putting it in
            argv or a file"
    (let [executor (shell/make-default-executor)
          result   (shell/run-cmd executor "cat" [] {:in "secret-passphrase"})]
      (is (= 0 (:exit result)))
      (is (= "secret-passphrase" (:out result))))))

(deftest default-executor-run-cmd-omits-in-when-not-given
  (testing "opts without :in behaves exactly as before (no stdin piped)"
    (let [executor (shell/make-default-executor)
          result   (shell/run-cmd executor "echo" ["hello"] {})]
      (is (= 0 (:exit result)))
      (is (= "hello\n" (:out result))))))
