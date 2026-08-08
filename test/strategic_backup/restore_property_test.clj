(ns strategic-backup.restore-property-test
  (:require [clojure.string :as str]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [strategic-backup.generators :as gens]
            [strategic-backup.restore :as restore]
            [strategic-backup.retention :as retention]))

;; ---------------------------------------------------------------------------
;; Property: resolve-manifest-mode always picks the correct tier (Req 7.1)
;; ---------------------------------------------------------------------------

(defspec resolve-manifest-mode-picks-correct-tier 100
  (prop/for-all [pg-manifest    (gen/one-of [(gen/return nil) gens/gen-manifest])
                 local-manifest (gen/one-of [(gen/return nil) gens/gen-manifest])
                 skip-verify?   gen/boolean]
    (let [{:keys [mode manifest]} (restore/resolve-manifest-mode pg-manifest local-manifest skip-verify?)]
      (cond
        skip-verify?            (and (= 3 mode) (nil? manifest))
        (some? pg-manifest)     (and (= 1 mode) (= pg-manifest manifest))
        (some? local-manifest)  (and (= 2 mode) (= local-manifest manifest))
        :else                   (and (= 3 mode) (nil? manifest))))))

;; ---------------------------------------------------------------------------
;; Property: latest-archive never picks something older than another
;; candidate (Req 7.3)
;; ---------------------------------------------------------------------------

(defspec latest-archive-is-not-older-than-any-other 100
  (prop/for-all [filenames (gen/not-empty gens/gen-filename-list)]
    (let [chosen    (restore/latest-archive filenames)
          chosen-ts (retention/parse-timestamp-from-filename chosen)
          all-ts    (map retention/parse-timestamp-from-filename filenames)]
      (every? #(<= (compare % chosen-ts) 0) all-ts))))

;; ---------------------------------------------------------------------------
;; Property: requires-openssl-decryption? matches the manifest's
;; :zfs-encrypted field in mode 1/2, and the filename's .enc suffix in
;; mode 3 (Req 7.7-7.9)
;; ---------------------------------------------------------------------------

(defspec requires-decryption-matches-manifest-field-in-mode-1-2 100
  (prop/for-all [manifest gens/gen-manifest
                 mode     (gen/elements [1 2])]
    (= (not (:zfs-encrypted manifest))
       (restore/requires-openssl-decryption? mode manifest (:archive-file manifest)))))

(defspec requires-decryption-matches-filename-suffix-in-mode-3 100
  (prop/for-all [manifest gens/gen-manifest]
    (= (str/ends-with? (:archive-file manifest) ".enc")
       (restore/requires-openssl-decryption? 3 nil (:archive-file manifest)))))
