(ns strategic-backup.retention-property-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [strategic-backup.generators :as gens]
            [strategic-backup.retention :as retention]))

;; ---------------------------------------------------------------------------
;; Property: sort-by-timestamp produces non-decreasing timestamp order
;; ---------------------------------------------------------------------------

(defspec sort-by-timestamp-produces-non-decreasing-order 100
  (prop/for-all [filenames gens/gen-filename-list]
    (let [timestamps (map retention/parse-timestamp-from-filename
                          (retention/sort-by-timestamp filenames))]
      (= timestamps (sort timestamps)))))

;; ---------------------------------------------------------------------------
;; Property: select-for-deletion preserves the total count
;; (every filename is either deleted or kept, exactly once)
;; ---------------------------------------------------------------------------

(defspec select-for-deletion-preserves-count 100
  (prop/for-all [filenames       gens/gen-filename-list
                 retention-count gens/gen-retention-count]
    (let [sorted    (retention/sort-by-timestamp filenames)
          to-delete (retention/select-for-deletion sorted retention-count)
          kept      (drop (count to-delete) sorted)]
      (= (count filenames) (+ (count to-delete) (count kept))))))

;; ---------------------------------------------------------------------------
;; Property: exactly the excess beyond retention-count is selected —
;; never more, never less
;; ---------------------------------------------------------------------------

(defspec select-for-deletion-matches-excess-exactly 100
  (prop/for-all [filenames       gens/gen-filename-list
                 retention-count gens/gen-retention-count]
    (let [sorted    (retention/sort-by-timestamp filenames)
          to-delete (retention/select-for-deletion sorted retention-count)]
      (= (count to-delete) (max 0 (- (count sorted) retention-count))))))

;; ---------------------------------------------------------------------------
;; Property: everything selected for deletion is at least as old as
;; everything kept
;; ---------------------------------------------------------------------------

(defspec select-for-deletion-deletes-oldest-first 100
  (prop/for-all [filenames       gens/gen-filename-list
                 retention-count gens/gen-retention-count]
    (let [sorted    (retention/sort-by-timestamp filenames)
          to-delete (retention/select-for-deletion sorted retention-count)
          kept      (drop (count to-delete) sorted)]
      (or (empty? to-delete)
          (empty? kept)
          (let [last-deleted-ts (retention/parse-timestamp-from-filename (last to-delete))
                first-kept-ts   (retention/parse-timestamp-from-filename (first kept))]
            (<= (compare last-deleted-ts first-kept-ts) 0))))))
