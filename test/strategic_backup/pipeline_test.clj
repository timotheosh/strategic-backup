(ns strategic-backup.pipeline-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [strategic-backup.generators :as gens]
            [strategic-backup.pipeline :as pipeline]))

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
