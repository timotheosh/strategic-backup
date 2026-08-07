(ns strategic-backup.manifest-test
  "Unit and property tests for the manifest namespace.

  **Validates: Requirements 2.4**"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.edn :as edn]
            [strategic-backup.generators :as gen]))

;; ---------------------------------------------------------------------------
;; Property 5: Manifest EDN Round-Trip
;;
;; For any valid manifest map with all required top-level fields, serializing
;; to EDN and parsing back SHALL produce a map equal under Clojure's `=`.
;;
;; **Validates: Requirements 2.4**
;; ---------------------------------------------------------------------------

(defspec manifest-edn-round-trip
  100
  (prop/for-all [manifest gen/gen-manifest]
    (= manifest (edn/read-string (pr-str manifest)))))
