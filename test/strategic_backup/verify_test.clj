(ns strategic-backup.verify-test
  (:require [clojure.test :refer [deftest testing is]]
            [strategic-backup.manifest]
            [strategic-backup.verify :as verify]))

;; ---------------------------------------------------------------------------
;; verify-stream-checksum?
;; ---------------------------------------------------------------------------

(deftest verify-stream-checksum-true-when-equal
  (is (true? (verify/verify-stream-checksum? "sha256:abc" "sha256:abc"))))

(deftest verify-stream-checksum-false-when-different
  (is (false? (verify/verify-stream-checksum? "sha256:abc" "sha256:def"))))

;; ---------------------------------------------------------------------------
;; diff-checksums
;; ---------------------------------------------------------------------------

(deftest diff-checksums-classifies-matched-mismatched-missing
  (let [expected {"./a.txt" "sha256:aaa"
                  "./b.txt" "sha256:bbb"
                  "./c.txt" "sha256:ccc"}
        actual   {"./a.txt" "sha256:aaa"          ; matches
                  "./b.txt" "sha256:different"}   ; mismatch; c.txt missing
        result   (verify/diff-checksums expected actual)]
    (is (= ["./a.txt"] (:matched result)))
    (is (= [{:path "./b.txt" :expected "sha256:bbb" :actual "sha256:different"}]
           (:mismatched result)))
    (is (= ["./c.txt"] (:missing result)))))

(deftest diff-checksums-all-matched-when-identical
  (let [m      {"./a.txt" "sha256:aaa" "./b.txt" "sha256:bbb"}
        result (verify/diff-checksums m m)]
    (is (= 2 (count (:matched result))))
    (is (empty? (:mismatched result)))
    (is (empty? (:missing result)))))

;; ---------------------------------------------------------------------------
;; compute-actual-checksums (delegates to manifest/compute-file-checksums)
;; ---------------------------------------------------------------------------

(deftest compute-actual-checksums-delegates-to-manifest
  (testing "delegates to manifest/compute-file-checksums (strict? hardcoded false)
            rather than reimplementing"
    (let [call-args (atom nil)]
      (with-redefs [strategic-backup.manifest/compute-file-checksums
                    (fn [mountpoint strict?]
                      (reset! call-args [mountpoint strict?])
                      {"./a.txt" "sha256:aaa"})]
        (let [result (verify/compute-actual-checksums "/mnt/test")]
          (is (= ["/mnt/test" false] @call-args))
          (is (= {"./a.txt" "sha256:aaa"} result)))))))

;; ---------------------------------------------------------------------------
;; verify-file-checksums!
;; ---------------------------------------------------------------------------

(deftest verify-file-checksums-ok-when-all-match
  (with-redefs [strategic-backup.manifest/compute-file-checksums
                (fn [_ _] {"./a.txt" "sha256:aaa"})]
    (let [result (verify/verify-file-checksums!
                  "/mnt/test"
                  {:snapshot "tank/x@backup-1" :files {"./a.txt" "sha256:aaa"}})]
      (is (true? (:ok result)))
      (is (= 1 (:matched result))))))

(deftest verify-file-checksums-not-ok-on-mismatch
  (with-redefs [strategic-backup.manifest/compute-file-checksums
                (fn [_ _] {"./a.txt" "sha256:wrong"})]
    (let [result (verify/verify-file-checksums!
                  "/mnt/test"
                  {:snapshot "tank/x@backup-1" :files {"./a.txt" "sha256:aaa"}})]
      (is (false? (:ok result)))
      (is (= 1 (count (:mismatched result)))))))

(deftest verify-file-checksums-not-ok-on-missing-file
  (with-redefs [strategic-backup.manifest/compute-file-checksums
                (fn [_ _] {})]
    (let [result (verify/verify-file-checksums!
                  "/mnt/test"
                  {:snapshot "tank/x@backup-1" :files {"./a.txt" "sha256:aaa"}})]
      (is (false? (:ok result)))
      (is (= ["./a.txt"] (:missing result))))))
