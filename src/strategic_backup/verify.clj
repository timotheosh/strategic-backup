(ns strategic-backup.verify
  "Restore integrity verification (Requirement 8)."
  (:require [strategic-backup.manifest :as manifest]
            [taoensso.timbre :as log]))

(defn verify-stream-checksum?
  "Calculation. True when the freshly computed archive checksum matches
   the manifest's :stream-checksum (Req 7.5)."
  [expected actual]
  (= expected actual))

(defn diff-checksums
  "Calculation. Compares `expected` (a manifest's :files map) against
   `actual` (freshly computed checksums), both keyed by relative file path.

   Returns:
     {:matched    [path ...]
      :mismatched [{:path .. :expected .. :actual ..} ...]
      :missing    [path ...]}   ; present in expected, absent from actual

   Every mismatch carries the expected hash, actual hash, and relative
   path (Req 8.1, 8.3)."
  [expected actual]
  (reduce
   (fn [acc [path expected-checksum]]
     (let [actual-checksum (get actual path)]
       (cond
         (nil? actual-checksum)
         (update acc :missing conj path)

         (= expected-checksum actual-checksum)
         (update acc :matched conj path)

         :else
         (update acc :mismatched conj {:path path :expected expected-checksum :actual actual-checksum}))))
   {:matched [] :mismatched [] :missing []}
   expected))

(defn compute-actual-checksums
  "Action. Computes SHA-256 checksums for every file under `mountpoint`
   (the restored Test_Dataset). Delegates to manifest/compute-file-checksums
   rather than re-walking the filesystem with separate logic (Req 8.1).
   strict? is hardcoded false — restore-test verification already has its
   own missing/mismatch reporting (diff-checksums below); the
   --strict-checksums flag is scoped to backup/--checksums only
   (pipeline-timing spec). `concurrency` is threaded through unchanged
   (Req 4.3) — restore-test verification walks a comparably large file
   set to backup and benefits equally from concurrent hashing."
  [mountpoint concurrency]
  (manifest/compute-file-checksums mountpoint false concurrency))

(defn verify-file-checksums!
  "Action. Computes actual checksums under `mountpoint` and compares them
   against `manifest`'s :files map, logging a detailed mismatch report on
   failure (Req 8.3) or a success summary including the verified file
   count (Req 8.5).

   Returns {:ok bool :matched N :mismatched [...] :missing [...]}."
  [mountpoint manifest concurrency]
  (let [expected (:files manifest)
        actual   (compute-actual-checksums mountpoint concurrency)
        diff     (diff-checksums expected actual)
        ok?      (and (empty? (:mismatched diff)) (empty? (:missing diff)))]
    (if ok?
      (log/info "Restore verification passed"
                {:snapshot       (:snapshot manifest)
                 :files-verified (count (:matched diff))})
      (do
        (doseq [{:keys [path expected actual]} (:mismatched diff)]
          (log/error "Checksum mismatch"
                     {:path path :expected expected :actual actual}))
        (when (seq (:missing diff))
          (log/error "Files missing from restored dataset" {:missing (:missing diff)}))))
    {:ok         ok?
     :matched    (count (:matched diff))
     :mismatched (:mismatched diff)
     :missing    (:missing diff)}))
