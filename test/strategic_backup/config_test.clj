(ns strategic-backup.config-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [strategic-backup.config :as config]
            [strategic-backup.generators :as gens])
  (:import (java.io File)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(def ^:private valid-config
  {:dataset                  "tank/syncthing"
   :test-dataset             "tank/restore-test"
   :staging-dir              "/var/backup/staging"
   :b2-bucket                "mybucket"
   :b2-path-prefix           "zfs-backups/"
   :retention-count          7
   :local-manifest-retention 14
   :compression              "gzip"
   :encryption-cipher        "aes-256-cbc"
   :snapshot-prefix          "backup"})

(defn- make-temp-file
  "Create a temp file with the given content. Caller must delete it."
  [content]
  (let [f (File/createTempFile "config-test" ".edn")]
    (spit f content)
    f))

;; ---------------------------------------------------------------------------
;; load-config tests
;; ---------------------------------------------------------------------------

(deftest load-config-throws-when-file-absent
  (testing "throws ex-info with :reason :file-not-found for a non-existent path"
    (let [path "/tmp/strategic-backup-no-such-file-xyz-123.edn"]
      (try
        (config/load-config path)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :file-not-found (:reason (ex-data e))))
          (is (= path (:path (ex-data e)))))))))

(deftest load-config-throws-on-invalid-edn
  (testing "throws ex-info with :reason :parse-error when file contains invalid EDN"
    (let [f (make-temp-file "{:key unclosed-map")]
      (try
        (config/load-config (.getAbsolutePath f))
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :parse-error (:reason (ex-data e)))))
        (finally (.delete f))))))

(deftest load-config-reads-valid-edn
  (testing "returns the parsed EDN map from a valid config file"
    (let [f (make-temp-file (pr-str valid-config))]
      (try
        (let [result (config/load-config (.getAbsolutePath f))]
          (is (= "tank/syncthing" (:dataset result)))
          (is (= 7 (:retention-count result))))
        (finally (.delete f))))))

;; ---------------------------------------------------------------------------
;; validate-config tests
;; ---------------------------------------------------------------------------

(deftest validate-config-returns-config-when-valid
  (testing "returns config unchanged when all required keys are present"
    (is (= valid-config (config/validate-config valid-config)))))

(deftest validate-config-reports-all-missing-keys
  (testing "throws ex-info with :missing-keys listing ALL absent keys simultaneously"
    (let [stripped (dissoc valid-config :dataset :retention-count :compression)]
      (try
        (config/validate-config stripped)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (let [missing (:missing-keys (ex-data e))]
            (is (vector? missing))
            ;; All three removed keys must appear — not just the first
            (is (some #{:dataset} missing))
            (is (some #{:retention-count} missing))
            (is (some #{:compression} missing))
            (is (= 3 (count missing)))))))))

(deftest validate-config-reports-single-missing-key
  (testing "throws when exactly one required key is absent"
    (let [stripped (dissoc valid-config :snapshot-prefix)]
      (try
        (config/validate-config stripped)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= [:snapshot-prefix] (:missing-keys (ex-data e)))))))))

(deftest validate-config-does-not-throw-on-extra-keys
  (testing "accepts config maps with additional non-required keys"
    (let [extended (assoc valid-config :extra-key "irrelevant")]
      (is (= extended (config/validate-config extended))))))

;; ---------------------------------------------------------------------------
;; resolve-secrets tests
;; ---------------------------------------------------------------------------

(deftest resolve-secrets-reports-all-missing-secrets
  (testing "throws ex-info with :missing-secrets listing ALL absent required secrets"
    ;; Override the internal getenv helper to simulate missing env vars
    (with-redefs [strategic-backup.config/getenv (constantly nil)]
      (try
        (config/resolve-secrets valid-config)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (let [missing (:missing-secrets (ex-data e))]
            (is (vector? missing))
            ;; Both required secrets must be reported
            (is (some #{"BACKUP_ENCRYPTION_KEY"} missing))
            (is (some #{"RCLONE_CONFIG"} missing))
            (is (= 2 (count missing)))))))))

(deftest resolve-secrets-reports-single-missing-secret
  (testing "throws when only one required secret is missing"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k]
                    (case k
                      "BACKUP_ENCRYPTION_KEY" "test-key-value"
                      nil))]
      (try
        (config/resolve-secrets valid-config)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (let [missing (:missing-secrets (ex-data e))]
            (is (= ["RCLONE_CONFIG"] missing))))))))

(deftest resolve-secrets-does-not-expose-secret-values
  (testing "exception message and ex-data contain only secret names, never values"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k]
                    (case k
                      "BACKUP_ENCRYPTION_KEY" "super-secret-key-abc123"
                      nil))]
      (try
        (config/resolve-secrets valid-config)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          ;; The exception message must not contain any secret value
          (is (not (clojure.string/includes? (.getMessage e) "super-secret-key-abc123")))
          ;; The ex-data must not contain any secret value
          (let [data-str (pr-str (ex-data e))]
            (is (not (clojure.string/includes? data-str "super-secret-key-abc123")))))))))

(deftest resolve-secrets-attaches-secrets-to-config
  (testing "returns config with :secrets map when all required secrets are present"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k]
                    (case k
                      "BACKUP_ENCRYPTION_KEY" "enc-key"
                      "RCLONE_CONFIG"         "/etc/rclone.conf"
                      "PGCONNSTRING"          "postgres://localhost/backups"
                      nil))]
      (let [result (config/resolve-secrets valid-config)]
        (is (map? (:secrets result)))
        (is (= "enc-key" (get-in result [:secrets :encryption-key])))
        (is (= "/etc/rclone.conf" (get-in result [:secrets :rclone-config])))
        (is (= "postgres://localhost/backups" (get-in result [:secrets :pg-conn-string])))))))

(deftest resolve-secrets-allows-missing-pgconnstring
  (testing "does not throw when PGCONNSTRING is absent — pg-conn-string is nil"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k]
                    (case k
                      "BACKUP_ENCRYPTION_KEY" "enc-key"
                      "RCLONE_CONFIG"         "/etc/rclone.conf"
                      nil))]
      (let [result (config/resolve-secrets valid-config)]
        (is (nil? (get-in result [:secrets :pg-conn-string])))))))

;; ---------------------------------------------------------------------------
;; Property 6: Config EDN Round-Trip
;; Validates: Requirements 11.2
;; ---------------------------------------------------------------------------

;; **Validates: Requirements 11.2**
(defspec config-edn-round-trip 100
  (prop/for-all [config gens/gen-config]
    (= config (edn/read-string (pr-str config)))))

;; ---------------------------------------------------------------------------
;; Property 7: Config Validation Reports All Missing Keys
;; Validates: Requirements 11.4
;; ---------------------------------------------------------------------------

(def ^:private required-config-keys
  [:dataset
   :test-dataset
   :staging-dir
   :b2-bucket
   :b2-path-prefix
   :retention-count
   :local-manifest-retention
   :compression
   :encryption-cipher
   :snapshot-prefix])

;; Generator: a non-empty subset of required config keys to remove
(def gen-keys-to-remove
  (gen/such-that seq (gen/fmap set (gen/not-empty (gen/list (gen/elements required-config-keys))))))

;; **Validates: Requirements 11.4**
(defspec config-validation-reports-all-missing-keys 100
  (prop/for-all [config       gens/gen-config
                 keys-to-drop gen-keys-to-remove]
    (let [incomplete (apply dissoc config keys-to-drop)]
      (try
        (config/validate-config incomplete)
        ;; Should never reach here — validate-config must throw when keys are missing
        false
        (catch clojure.lang.ExceptionInfo e
          (let [missing-keys (set (:missing-keys (ex-data e)))]
            ;; Every key we removed must appear in :missing-keys
            (every? #(contains? missing-keys %) keys-to-drop)))))))

;; ---------------------------------------------------------------------------
;; Property 8: Secret Validation Reports All Missing Secrets
;; Validates: Requirements 9.3
;; ---------------------------------------------------------------------------

;; The two required secrets. PGCONNSTRING is optional and must never appear
;; in :missing-secrets even when absent.
(def ^:private required-secret-names
  ["BACKUP_ENCRYPTION_KEY" "RCLONE_CONFIG"])

;; Generator: a non-empty subset of the required secret names to withhold
(def gen-secrets-to-withhold
  (gen/such-that seq
                 (gen/fmap set
                           (gen/not-empty (gen/list (gen/elements required-secret-names))))))

;; **Validates: Requirements 9.3**
(defspec secret-validation-reports-all-missing-secrets 100
  (prop/for-all [config           gens/gen-config
                 secrets-to-omit  gen-secrets-to-withhold]
    ;; Build a mock env that has every required secret EXCEPT those in secrets-to-omit.
    ;; PGCONNSTRING is always absent so we can confirm it never leaks into :missing-secrets.
    (let [present-secrets (remove secrets-to-omit required-secret-names)
          mock-env        (into {} (map (fn [k] [k (str "mock-value-" k)]) present-secrets))]
      (with-redefs [strategic-backup.config/getenv (fn [k] (get mock-env k))]
        (try
          (config/resolve-secrets config)
          ;; resolve-secrets must throw when any required secret is absent
          false
          (catch clojure.lang.ExceptionInfo e
            (let [missing-reported (set (:missing-secrets (ex-data e)))]
              ;; Every omitted required secret must appear in :missing-secrets
              (and (every? #(contains? missing-reported %) secrets-to-omit)
                   ;; PGCONNSTRING is optional — it must never appear in :missing-secrets
                   (not (contains? missing-reported "PGCONNSTRING"))
                   ;; :missing-secrets must be a non-empty vector/seq
                   (seq missing-reported)))))))))
