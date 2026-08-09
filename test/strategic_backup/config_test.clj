(ns strategic-backup.config-test
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [strategic-backup.config :as config]
            [strategic-backup.generators :as gens]
            [strategic-backup.infisical :as infisical])
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
;; infisical-mode? / missing-infisical-config-keys
;; (infisical-secrets spec, Requirement 1)
;; ---------------------------------------------------------------------------

(deftest infisical-mode-true-only-when-secret-store-is-infisical
  (testing "true when :secret-store is :infisical"
    (is (true? (config/infisical-mode? {:secret-store :infisical}))))
  (testing "false when :secret-store is absent"
    (is (false? (config/infisical-mode? {}))))
  (testing "false when :secret-store is some other value"
    (is (false? (config/infisical-mode? {:secret-store :vault})))))

(deftest missing-infisical-config-keys-reports-absent-project-id
  (testing "reports :project-id when absent"
    (is (= [:project-id] (config/missing-infisical-config-keys {}))))
  (testing "reports :project-id when the :infisical map itself is nil"
    (is (= [:project-id] (config/missing-infisical-config-keys nil))))
  (testing "empty when :project-id present"
    (is (= [] (config/missing-infisical-config-keys {:project-id "proj-123"})))))

;; ---------------------------------------------------------------------------
;; validate-config: :infisical map validation (Requirement 1.3)
;; ---------------------------------------------------------------------------

(deftest validate-config-passes-through-unchanged-when-secret-store-absent
  (testing "byte-identical behavior to before this feature existed (Requirement 5.2)"
    (is (= valid-config (config/validate-config valid-config)))))

(deftest validate-config-throws-for-missing-infisical-project-id
  (testing "throws a separate ex-info when :secret-store is :infisical and :project-id is missing"
    (let [cfg (assoc valid-config :secret-store :infisical :infisical {})]
      (try
        (config/validate-config cfg)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= [:project-id] (:missing-infisical-keys (ex-data e))))))))
  (testing "throws when the :infisical map itself is entirely absent"
    (let [cfg (assoc valid-config :secret-store :infisical)]
      (try
        (config/validate-config cfg)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= [:project-id] (:missing-infisical-keys (ex-data e)))))))))

(deftest validate-config-passes-when-infisical-project-id-present
  (testing "does not throw when :secret-store is :infisical and :project-id is present"
    (let [cfg (assoc valid-config :secret-store :infisical :infisical {:project-id "proj-123"})]
      (is (= cfg (config/validate-config cfg))))))

;; ---------------------------------------------------------------------------
;; credential-source (infisical-secrets spec, Requirements 2.1-2.2, 3.1-3.2)
;; ---------------------------------------------------------------------------

(deftest credential-source-env-wins-regardless-of-infisical-mode
  (testing "env value present => :env, even when infisical-mode? is true (legacy precedence, Core Principle)"
    (is (= :env (config/credential-source "some-value" true)))
    (is (= :env (config/credential-source "some-value" false)))))

(deftest credential-source-infisical-when-env-absent-and-mode-on
  (is (= :infisical (config/credential-source nil true))))

(deftest credential-source-missing-when-env-absent-and-mode-off
  (is (= :missing (config/credential-source nil false))))

;; ---------------------------------------------------------------------------
;; resolve-db-credential! (infisical-secrets spec, Requirements 2.1-2.5)
;; NEVER throws — DB persistence stays optional under Infisical mode too.
;; ---------------------------------------------------------------------------

(deftest resolve-db-credential-legacy-env-wins-without-calling-infisical
  (testing "returns the env value directly, never calls infisical/fetch-secret!"
    (let [fetch-called (atom false)]
      (with-redefs [infisical/fetch-secret! (fn [_ _] (reset! fetch-called true) "should-not-be-used")]
        (let [result (config/resolve-db-credential!
                      {"PGCONNSTRING" "postgres://legacy"}
                      {:project-id "proj-123"})]
          (is (= "postgres://legacy" result))
          (is (false? @fetch-called)))))))

(deftest resolve-db-credential-fetches-from-infisical-when-env-absent
  (testing "calls infisical/fetch-secret! with PGCONNSTRING when the legacy env var is absent"
    (with-redefs [infisical/fetch-secret!
                  (fn [infisical-config secret-name]
                    (is (= "proj-123" (:project-id infisical-config)))
                    (is (= "PGCONNSTRING" secret-name))
                    "postgres://from-infisical")]
      (let [result (config/resolve-db-credential! {} {:project-id "proj-123"})]
        (is (= "postgres://from-infisical" result))))))

(deftest resolve-db-credential-nil-when-absent-and-not-infisical-mode
  (testing "returns nil without throwing when env absent and not in Infisical mode (unchanged legacy behavior)"
    (is (nil? (config/resolve-db-credential! {} nil)))))

(deftest resolve-db-credential-nil-when-infisical-fetch-fails
  (testing "returns nil without throwing when the Infisical fetch itself fails, for any reason (Requirement 2.3/2.5)"
    (with-redefs [infisical/fetch-secret!
                  (fn [_ _] (throw (ex-info "Infisical secret fetch failed for PGCONNSTRING"
                                            {:stage :secrets :secret-name "PGCONNSTRING"
                                             :infisical-error :clj-infisical/secret-not-found})))]
      (is (nil? (config/resolve-db-credential! {} {:project-id "proj-123"}))))))

;; ---------------------------------------------------------------------------
;; resolve-b2-credential! (infisical-secrets spec, Requirements 3.1-3.4)
;; Always throws on failure — B2 access is required for the pipeline to work.
;; ---------------------------------------------------------------------------

(deftest resolve-b2-credential-legacy-env-wins-without-calling-infisical
  (testing "returns {:mode :rclone-config-file}, never calls infisical/fetch-secret!"
    (let [fetch-called (atom false)]
      (with-redefs [infisical/fetch-secret! (fn [_ _] (reset! fetch-called true) "should-not-be-used")]
        (let [result (config/resolve-b2-credential!
                      {"RCLONE_CONFIG" "/etc/rclone.conf"}
                      {:project-id "proj-123"})]
          (is (= {:mode :rclone-config-file} result))
          (is (false? @fetch-called)))))))

(deftest resolve-b2-credential-fetches-both-secrets-from-infisical-when-env-absent
  (testing "fetches B2_KEY_ID and B2_APPLICATION_KEY, returns {:mode :infisical :env {...}}"
    (with-redefs [infisical/fetch-secret!
                  (fn [_ secret-name]
                    (case secret-name
                      "B2_KEY_ID"          "my-key-id"
                      "B2_APPLICATION_KEY" "my-app-key"))]
      (let [result (config/resolve-b2-credential! {} {:project-id "proj-123"})]
        (is (= :infisical (:mode result)))
        (is (= {"RCLONE_CONFIG_B2_TYPE"    "b2"
                "RCLONE_CONFIG_B2_ACCOUNT" "my-key-id"
                "RCLONE_CONFIG_B2_KEY"     "my-app-key"}
               (:env result)))))))

(deftest resolve-b2-credential-throws-when-absent-and-not-infisical-mode
  (testing "throws ex-info :stage :secrets when env absent and not in Infisical mode"
    (try
      (config/resolve-b2-credential! {} nil)
      (is false "expected ex-info to be thrown")
      (catch clojure.lang.ExceptionInfo e
        (is (= :secrets (:stage (ex-data e))))))))

(deftest resolve-b2-credential-throws-when-infisical-fetch-fails
  (testing "throws (does not swallow) when the Infisical fetch itself fails, for any reason (Requirement 3.4)"
    (with-redefs [infisical/fetch-secret!
                  (fn [_ secret-name]
                    (throw (ex-info (str "Infisical secret fetch failed for " secret-name)
                                    {:stage :secrets :secret-name secret-name
                                     :infisical-error :clj-infisical/secret-not-found})))]
      (try
        (config/resolve-b2-credential! {} {:project-id "proj-123"})
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :secrets (:stage (ex-data e)))))))))

;; ---------------------------------------------------------------------------
;; resolve-zfs-passphrase! (ZFS encryption passphrase, used to zfs load-key
;; a received test-dataset during restore-test verification)
;; NEVER throws AND never logs — unlike PGCONNSTRING, this secret is purely
;; situational (only ZFS-encrypted datasets need it); restore.clj is
;; responsible for failing loudly at the point of actual use.
;; ---------------------------------------------------------------------------

(deftest resolve-zfs-passphrase-legacy-env-wins-without-calling-infisical
  (testing "returns the env value directly, never calls infisical/fetch-secret!"
    (let [fetch-called (atom false)]
      (with-redefs [infisical/fetch-secret! (fn [_ _] (reset! fetch-called true) "should-not-be-used")]
        (let [result (config/resolve-zfs-passphrase!
                      {"ZFS_ENCRYPTION_PASSPHRASE" "legacy-pass"}
                      {:project-id "proj-123"})]
          (is (= "legacy-pass" result))
          (is (false? @fetch-called)))))))

(deftest resolve-zfs-passphrase-fetches-from-infisical-when-env-absent
  (testing "calls infisical/fetch-secret! with ZFS_ENCRYPTION_PASSPHRASE when the legacy env var is absent"
    (with-redefs [infisical/fetch-secret!
                  (fn [infisical-config secret-name]
                    (is (= "proj-123" (:project-id infisical-config)))
                    (is (= "ZFS_ENCRYPTION_PASSPHRASE" secret-name))
                    "pass-from-infisical")]
      (let [result (config/resolve-zfs-passphrase! {} {:project-id "proj-123"})]
        (is (= "pass-from-infisical" result))))))

(deftest resolve-zfs-passphrase-nil-when-absent-and-not-infisical-mode
  (testing "returns nil without throwing when env absent and not in Infisical mode"
    (is (nil? (config/resolve-zfs-passphrase! {} nil)))))

(deftest resolve-zfs-passphrase-nil-when-infisical-fetch-fails
  (testing "returns nil without throwing when the Infisical fetch itself fails, for any reason"
    (with-redefs [infisical/fetch-secret!
                  (fn [_ _] (throw (ex-info "Infisical secret fetch failed for ZFS_ENCRYPTION_PASSPHRASE"
                                            {:stage :secrets :secret-name "ZFS_ENCRYPTION_PASSPHRASE"
                                             :infisical-error :clj-infisical/secret-not-found})))]
      (is (nil? (config/resolve-zfs-passphrase! {} {:project-id "proj-123"}))))))

;; ---------------------------------------------------------------------------
;; resolve-secrets tests
;; ---------------------------------------------------------------------------

(deftest resolve-secrets-reports-missing-rclone-config
  (testing "throws ex-info with :missing-secrets when RCLONE_CONFIG is absent —
            the sole required secret now that BACKUP_ENCRYPTION_KEY is
            conditionally required (only when the dataset isn't ZFS-encrypted,
            checked at point of use in core.clj/restore.clj, not here)"
    (with-redefs [strategic-backup.config/getenv (constantly nil)]
      (try
        (config/resolve-secrets valid-config)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= ["RCLONE_CONFIG"] (:missing-secrets (ex-data e)))))))))

(deftest resolve-secrets-does-not-require-backup-encryption-key
  (testing "does not throw when BACKUP_ENCRYPTION_KEY is absent but RCLONE_CONFIG
            is present — :encryption-key is simply nil in :secrets"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k]
                    (case k
                      "RCLONE_CONFIG" "/etc/rclone.conf"
                      nil))]
      (let [result (config/resolve-secrets valid-config)]
        (is (nil? (get-in result [:secrets :encryption-key])))))))

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

(deftest resolve-secrets-legacy-mode-attaches-zfs-passphrase-from-env
  (testing "legacy mode reads ZFS_ENCRYPTION_PASSPHRASE straight from the environment"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k]
                    (case k
                      "BACKUP_ENCRYPTION_KEY"       "enc-key"
                      "RCLONE_CONFIG"               "/etc/rclone.conf"
                      "ZFS_ENCRYPTION_PASSPHRASE"   "legacy-pass"
                      nil))]
      (let [result (config/resolve-secrets valid-config)]
        (is (= "legacy-pass" (get-in result [:secrets :zfs-encryption-passphrase])))))))

(deftest resolve-secrets-legacy-mode-allows-missing-zfs-passphrase
  (testing "does not throw when ZFS_ENCRYPTION_PASSPHRASE is absent — nil, no warning required"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k]
                    (case k
                      "BACKUP_ENCRYPTION_KEY" "enc-key"
                      "RCLONE_CONFIG"         "/etc/rclone.conf"
                      nil))]
      (let [result (config/resolve-secrets valid-config)]
        (is (nil? (get-in result [:secrets :zfs-encryption-passphrase])))))))

;; ---------------------------------------------------------------------------
;; resolve-secrets: Infisical mode (infisical-secrets spec, Requirements 2, 3, 5)
;; ---------------------------------------------------------------------------

(def ^:private infisical-config
  (assoc valid-config :secret-store :infisical :infisical {:project-id "proj-123"}))

(deftest resolve-secrets-legacy-only-mode-unchanged-shape
  (testing "when :secret-store is absent, :secrets shape is exactly as before the
            Infisical feature existed, plus :zfs-encryption-passphrase — which,
            like PGCONNSTRING, is resolvable from a plain env var regardless of
            Infisical involvement (Requirement 5.1, 5.2; Requirement 6)"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k] (case k "BACKUP_ENCRYPTION_KEY" "enc-key" "RCLONE_CONFIG" "/etc/rclone.conf" nil))]
      (let [result (config/resolve-secrets valid-config)]
        (is (= #{:encryption-key :rclone-config :pg-conn-string :zfs-encryption-passphrase}
               (set (keys (:secrets result)))))))))

(deftest resolve-secrets-infisical-mode-attaches-pg-conn-string-and-b2-rclone-env
  (testing "adds :pg-conn-string (via Infisical) and :b2-rclone-env when both legacy env vars are absent"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k] (case k "BACKUP_ENCRYPTION_KEY" "enc-key" nil))
                  infisical/fetch-secret!
                  (fn [_ secret-name]
                    (case secret-name
                      "PGCONNSTRING"       "postgres://from-infisical"
                      "B2_KEY_ID"          "key-id"
                      "B2_APPLICATION_KEY" "app-key"
                      nil))]
      (let [result (config/resolve-secrets infisical-config)]
        (is (= "postgres://from-infisical" (get-in result [:secrets :pg-conn-string])))
        (is (= {"RCLONE_CONFIG_B2_TYPE"    "b2"
                "RCLONE_CONFIG_B2_ACCOUNT" "key-id"
                "RCLONE_CONFIG_B2_KEY"     "app-key"}
               (get-in result [:secrets :b2-rclone-env])))))))

(deftest resolve-secrets-infisical-mode-legacy-rclone-config-still-wins
  (testing "when RCLONE_CONFIG is present even in Infisical mode, :b2-rclone-env is {} (legacy file path used)"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k] (case k "BACKUP_ENCRYPTION_KEY" "enc-key" "RCLONE_CONFIG" "/etc/rclone.conf" nil))]
      (let [result (config/resolve-secrets infisical-config)]
        (is (= {} (get-in result [:secrets :b2-rclone-env])))
        (is (= "/etc/rclone.conf" (get-in result [:secrets :rclone-config])))))))

(deftest resolve-secrets-infisical-mode-pg-failure-does-not-abort
  (testing "an Infisical PGCONNSTRING fetch failure does not abort — :pg-conn-string is simply nil (Requirement 2.3/2.5)"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k] (case k "BACKUP_ENCRYPTION_KEY" "enc-key" "RCLONE_CONFIG" "/etc/rclone.conf" nil))
                  infisical/fetch-secret!
                  (fn [_ _] (throw (ex-info "not found" {:type :clj-infisical/secret-not-found})))]
      (let [result (config/resolve-secrets infisical-config)]
        (is (nil? (get-in result [:secrets :pg-conn-string])))))))

(deftest resolve-secrets-infisical-mode-b2-failure-aborts
  (testing "an Infisical B2 fetch failure with no legacy fallback DOES abort (Requirement 3.4)"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k] (case k "BACKUP_ENCRYPTION_KEY" "enc-key" nil))
                  infisical/fetch-secret!
                  (fn [_ secret-name]
                    (case secret-name
                      "PGCONNSTRING" "postgres://from-infisical"
                      (throw (ex-info (str "Infisical secret fetch failed for " secret-name)
                                      {:stage :secrets :secret-name secret-name
                                       :infisical-error :clj-infisical/secret-not-found}))))]
      (try
        (config/resolve-secrets infisical-config)
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :secrets (:stage (ex-data e)))))))))

(deftest resolve-secrets-infisical-mode-attaches-zfs-passphrase-via-infisical
  (testing "adds :zfs-encryption-passphrase via Infisical when the legacy env var is absent"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k] (case k "BACKUP_ENCRYPTION_KEY" "enc-key" nil))
                  infisical/fetch-secret!
                  (fn [_ secret-name]
                    (case secret-name
                      "PGCONNSTRING"               "postgres://from-infisical"
                      "B2_KEY_ID"                   "key-id"
                      "B2_APPLICATION_KEY"          "app-key"
                      "ZFS_ENCRYPTION_PASSPHRASE"   "pass-from-infisical"))]
      (let [result (config/resolve-secrets infisical-config)]
        (is (= "pass-from-infisical" (get-in result [:secrets :zfs-encryption-passphrase])))))))

(deftest resolve-secrets-infisical-mode-zfs-passphrase-failure-does-not-abort
  (testing "an Infisical ZFS_ENCRYPTION_PASSPHRASE fetch failure does not abort — nil, same as PGCONNSTRING"
    (with-redefs [strategic-backup.config/getenv
                  (fn [k] (case k "BACKUP_ENCRYPTION_KEY" "enc-key" "RCLONE_CONFIG" "/etc/rclone.conf" nil))
                  infisical/fetch-secret!
                  (fn [_ secret-name]
                    (case secret-name
                      "PGCONNSTRING" "postgres://from-infisical"
                      (throw (ex-info "not found" {:type :clj-infisical/secret-not-found}))))]
      (let [result (config/resolve-secrets infisical-config)]
        (is (nil? (get-in result [:secrets :zfs-encryption-passphrase])))))))

(deftest resolve-secrets-infisical-mode-does-not-require-encryption-key
  (testing "BACKUP_ENCRYPTION_KEY is not required upfront in Infisical mode either —
            it's only actually needed when the dataset turns out not to be
            ZFS-encrypted, checked at point of use in core.clj/restore.clj, not here"
    (with-redefs [strategic-backup.config/getenv (constantly nil)
                  infisical/fetch-secret!        (constantly nil)]
      (let [result (config/resolve-secrets infisical-config)]
        (is (nil? (get-in result [:secrets :encryption-key])))))))

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

;; The sole unconditionally-required secret. BACKUP_ENCRYPTION_KEY,
;; PGCONNSTRING, and ZFS_ENCRYPTION_PASSPHRASE are all optional at this
;; layer — they must never appear in :missing-secrets even when absent
;; (BACKUP_ENCRYPTION_KEY's requiredness is conditional on the dataset's
;; ZFS-encryption status, checked at point of use in core.clj/restore.clj).
(def ^:private required-secret-names
  ["RCLONE_CONFIG"])

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
                   ;; PGCONNSTRING and BACKUP_ENCRYPTION_KEY are optional at
                   ;; this layer — neither may ever appear in :missing-secrets
                   (not (contains? missing-reported "PGCONNSTRING"))
                   (not (contains? missing-reported "BACKUP_ENCRYPTION_KEY"))
                   ;; :missing-secrets must be a non-empty vector/seq
                   (seq missing-reported)))))))))

;; ---------------------------------------------------------------------------
;; Property 1 (infisical-secrets spec): Credential Source Decision Table
;; Property 2 (infisical-secrets spec): Legacy Precedence Is Absolute
;; ---------------------------------------------------------------------------

;; **Validates: Requirements 2.1, 2.2, 3.1, 3.2**
(defspec credential-source-decision-table 100
  (prop/for-all [env-present?    gen/boolean
                 env-value       gen/string-alphanumeric
                 infisical-mode? gen/boolean]
    (let [env-value' (when env-present? env-value)
          result     (config/credential-source env-value' infisical-mode?)]
      (cond
        env-present?                    (= :env result)
        (and (not env-present?) infisical-mode?) (= :infisical result)
        :else                           (= :missing result)))))

;; **Validates: Requirement 2.1, 3.1 (Core Principle — legacy precedence is absolute)**
(defspec credential-source-legacy-precedence-is-absolute 100
  (prop/for-all [env-value       (gen/not-empty gen/string-alphanumeric)
                 infisical-mode? gen/boolean]
    (not= :infisical (config/credential-source env-value infisical-mode?))))
