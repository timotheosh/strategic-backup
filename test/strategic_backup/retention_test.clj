(ns strategic-backup.retention-test
  (:require [clojure.test :refer [deftest testing is]]
            [strategic-backup.retention :as retention]))

;; ---------------------------------------------------------------------------
;; parse-timestamp-from-filename
;; ---------------------------------------------------------------------------

(deftest parse-timestamp-from-filename-extracts-embedded-timestamp
  (testing "extracts timestamp from an unencrypted archive filename"
    (is (= "2026-05-11T020000"
           (retention/parse-timestamp-from-filename "tank-syncthing-2026-05-11T020000.zfs.gz"))))
  (testing "extracts timestamp from an encrypted archive filename"
    (is (= "2025-01-01T000000"
           (retention/parse-timestamp-from-filename "pool-data-2025-01-01T000000.zfs.gz.enc")))))

(deftest parse-timestamp-from-filename-nil-when-absent
  (testing "returns nil for filenames without an embedded timestamp"
    (is (nil? (retention/parse-timestamp-from-filename "not-an-archive.txt")))))

;; ---------------------------------------------------------------------------
;; sort-by-timestamp
;; ---------------------------------------------------------------------------

(deftest sort-by-timestamp-orders-oldest-first
  (testing "sorts filenames ascending by embedded timestamp"
    (let [files ["a-2026-03-01T000000.zfs.gz"
                 "a-2024-01-01T000000.zfs.gz"
                 "a-2025-06-15T120000.zfs.gz"]]
      (is (= ["a-2024-01-01T000000.zfs.gz"
              "a-2025-06-15T120000.zfs.gz"
              "a-2026-03-01T000000.zfs.gz"]
             (retention/sort-by-timestamp files))))))

;; ---------------------------------------------------------------------------
;; select-for-deletion
;; ---------------------------------------------------------------------------

(deftest select-for-deletion-returns-oldest-excess
  (testing "keeps the newest retention-count, selects the rest for deletion"
    (let [sorted ["a" "b" "c" "d" "e"]]
      (is (= ["a" "b"] (retention/select-for-deletion sorted 3))))))

(deftest select-for-deletion-empty-when-within-retention
  (testing "returns empty when count <= retention-count"
    (is (= [] (retention/select-for-deletion ["a" "b"] 5)))
    (is (= [] (retention/select-for-deletion ["a" "b"] 2)))))

;; ---------------------------------------------------------------------------
;; enforce-retention!
;; ---------------------------------------------------------------------------

(deftest enforce-retention-deletes-oldest-excess
  (testing "calls delete-remote! for each selected filename and reports :deleted"
    (let [deleted-calls (atom [])]
      (with-redefs [strategic-backup.upload/delete-remote!
                    (fn [_ _ filename]
                      (swap! deleted-calls conj filename)
                      {:ok true})]
        (let [result (retention/enforce-retention!
                      nil "b2:bucket" ["a" "b" "c" "d"] 2)]
          (is (= ["a" "b"] @deleted-calls))
          (is (= ["a" "b"] (:deleted result)))
          (is (= [] (:failed result))))))))

(deftest enforce-retention-continues-past-individual-failures
  (testing "aggregates failures without aborting remaining deletions"
    (with-redefs [strategic-backup.upload/delete-remote!
                  (fn [_ _ filename]
                    (if (= filename "a")
                      {:ok false :error "network error"}
                      {:ok true}))]
      (let [result (retention/enforce-retention!
                    nil "b2:bucket" ["a" "b" "c"] 1)]
        (is (= ["b"] (:deleted result)))
        (is (= [{:filename "a" :error "network error"}] (:failed result)))))))

(deftest enforce-retention-deletes-nothing-when-within-retention
  (testing "no deletions attempted when archive count is within retention"
    (let [calls (atom 0)]
      (with-redefs [strategic-backup.upload/delete-remote!
                    (fn [_ _ _] (swap! calls inc) {:ok true})]
        (let [result (retention/enforce-retention! nil "b2:bucket" ["a" "b"] 5)]
          (is (= 0 @calls))
          (is (= {:deleted [] :failed []} result)))))))
