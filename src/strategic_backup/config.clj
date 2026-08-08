(ns strategic-backup.config
  "Configuration loading, validation, and secret resolution.

   Config is loaded from an EDN file whose path is given by the
   STRATEGIC_BACKUP_CONFIG environment variable, defaulting to
   /etc/strategic-backup/config.edn.

   Secrets are read exclusively from environment variables and are
   never logged or included in exception messages."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [strategic-backup.infisical :as infisical]
            [taoensso.timbre :as log]))

;; ---------------------------------------------------------------------------
;; Required config keys
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

;; ---------------------------------------------------------------------------
;; Required secret env-var names
;; ---------------------------------------------------------------------------

(def ^:private required-secret-env-vars
  ["BACKUP_ENCRYPTION_KEY" "RCLONE_CONFIG"])

(def ^:private optional-secret-env-vars
  ["PGCONNSTRING"])

;; ---------------------------------------------------------------------------
;; Required :infisical config-map keys (infisical-secrets spec, Requirement 1)
;; ---------------------------------------------------------------------------

(def ^:private required-infisical-config-keys
  [:project-id])

;; ---------------------------------------------------------------------------
;; Internal env-var lookup (rebindable for testing)
;; ---------------------------------------------------------------------------

(defn getenv
  "Thin wrapper around System/getenv to allow rebinding in tests.
   Not part of the public API — use config/resolve-secrets instead."
  [k]
  (System/getenv k))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn resolve-config-path
  "Calculation. Picks the effective config file path: explicit `path` wins,
   then `env-path` (the STRATEGIC_BACKUP_CONFIG value), then `default-path`."
  [path env-path default-path]
  (or path env-path default-path))

(defn load-config
  "Load the EDN config map from `path`.

   If `path` is nil, the path is resolved from the STRATEGIC_BACKUP_CONFIG
   environment variable, falling back to /etc/strategic-backup/config.edn.

   Throws ex-info with:
     :reason :file-not-found  — when the file does not exist
     :reason :parse-error     — when the file content is not valid EDN"
  [path]
  (let [resolved-path (resolve-config-path
                       path
                       (getenv "STRATEGIC_BACKUP_CONFIG")
                       "/etc/strategic-backup/config.edn")
        f             (io/file resolved-path)]
    (when-not (.exists f)
      (throw (ex-info (str "Config file not found: " resolved-path)
                      {:reason :file-not-found
                       :path   resolved-path})))
    (try
      (edn/read-string (slurp f))
      (catch Exception e
        (throw (ex-info (str "Failed to parse config file: " resolved-path)
                        {:reason    :parse-error
                         :path      resolved-path
                         :cause-msg (.getMessage e)}
                        e))))))

(defn missing-config-keys
  "Calculation. Returns a vector of all keys from `required-config-keys`
   that are absent from `config`."
  [config]
  (filterv #(not (contains? config %)) required-config-keys))

(defn infisical-mode?
  "Calculation. True only when :secret-store is explicitly :infisical
   (infisical-secrets spec, Requirement 1.1)."
  [config]
  (= :infisical (:secret-store config)))

(defn missing-infisical-config-keys
  "Calculation. Returns a vector of all keys from
   `required-infisical-config-keys` absent from `infisical-config` (which
   may itself be nil — treated the same as an empty map)."
  [infisical-config]
  (filterv #(not (contains? infisical-config %)) required-infisical-config-keys))

(defn validate-config
  "Validate that `config` contains all required keys.

   Returns `config` unchanged when valid.

   Throws ex-info with :missing-keys containing a vector of ALL absent
   required keys (not just the first one encountered)."
  [config]
  (let [missing (missing-config-keys config)]
    (when (seq missing)
      (log/error "Config validation failed. Missing keys:" missing)
      (throw (ex-info "Config is missing required keys"
                      {:missing-keys missing}))))
  ;; infisical-secrets spec, Requirement 1.3. Only ever runs when
  ;; :secret-store is :infisical — when absent (the legacy-only case),
  ;; this block is skipped entirely, so behavior is byte-identical to
  ;; before this feature existed (Requirement 5.2).
  (when (infisical-mode? config)
    (let [missing-infisical (missing-infisical-config-keys (:infisical config))]
      (when (seq missing-infisical)
        (log/error "Config validation failed. Missing Infisical config keys:" missing-infisical)
        (throw (ex-info "Config is missing required Infisical keys"
                        {:missing-infisical-keys missing-infisical})))))
  config)

(defn credential-source
  "Calculation — the credential-source decision table (infisical-secrets
   spec, Requirements 2.1-2.2, 3.1-3.2). A present `env-value` always wins
   regardless of `infisical-mode?` (legacy precedence, the feature's Core
   Principle). Shared by both the database and B2 credential resolvers —
   the decision logic is identical for both."
  [env-value infisical-mode?]
  (cond
    (some? env-value) :env
    infisical-mode?   :infisical
    :else              :missing))

(defn resolve-db-credential!
  "Action (infisical-secrets spec, Requirements 2.1-2.5). Resolves the
   database connection string. `env-map` has \"PGCONNSTRING\" already read
   via getenv; `infisical-config` is the :infisical config map, or nil
   when not in Infisical mode.

   NEVER throws — a legacy env value wins if present; otherwise, when in
   Infisical mode, fetches secret \"PGCONNSTRING\". Any failure to resolve
   a value at all — env absent and not in Infisical mode, OR the Infisical
   fetch itself failing for any reason (not-found, auth failure, network
   error, ...) — logs a warning and returns nil, exactly like today's
   PGCONNSTRING-absent behavior. Database persistence stays optional even
   under Infisical mode — the base spec's Core Principle must hold here."
  [env-map infisical-config]
  (let [env-value (get env-map "PGCONNSTRING")]
    (case (credential-source env-value (some? infisical-config))
      :env
      env-value

      :infisical
      (try
        (infisical/fetch-secret! infisical-config "PGCONNSTRING")
        (catch clojure.lang.ExceptionInfo e
          (log/warn "Infisical fetch for PGCONNSTRING failed — PostgreSQL persistence will be skipped:"
                    (.getMessage e))
          nil))

      :missing
      (do
        (log/warn "PGCONNSTRING not set — PostgreSQL persistence will be skipped")
        nil))))

(defn resolve-b2-credential!
  "Action (infisical-secrets spec, Requirements 3.1-3.4). Resolves B2
   credentials. `env-map` has \"RCLONE_CONFIG\" already read via getenv;
   `infisical-config` is the :infisical config map, or nil when not in
   Infisical mode.

   Unlike resolve-db-credential! above, this always fails loudly — B2
   access is required for the backup pipeline to function at all. Throws
   ex-info {:stage :secrets} when there's no legacy fallback and either
   Infisical mode isn't active, or the Infisical fetch itself fails."
  [env-map infisical-config]
  (let [env-value (get env-map "RCLONE_CONFIG")]
    (case (credential-source env-value (some? infisical-config))
      :env
      {:mode :rclone-config-file}

      :infisical
      (let [key-id  (infisical/fetch-secret! infisical-config "B2_KEY_ID")
            app-key (infisical/fetch-secret! infisical-config "B2_APPLICATION_KEY")]
        {:mode :infisical :env (infisical/b2-rclone-env-vars key-id app-key)})

      :missing
      (throw (ex-info "Unable to resolve B2 credential — no RCLONE_CONFIG and Infisical mode not active"
                      {:stage :secrets :credential "B2_KEY_ID"})))))

(defn read-secret-env-vars
  "Reads every required and optional secret env var (via `getenv`) into a
   plain map of env-var-name -> value-or-nil. The only thing in this
   namespace that touches `getenv` for secrets."
  []
  (into {} (map (fn [k] [k (getenv k)])
                (concat required-secret-env-vars optional-secret-env-vars))))

(defn missing-required-secrets
  "Calculation. Returns a vector of required secret names whose value in
   `env-map` is nil."
  [env-map]
  (filterv #(nil? (get env-map %)) required-secret-env-vars))

(defn build-secrets-map
  "Calculation. Shapes the raw env-var map into the :secrets map attached
   to config: {:encryption-key ... :rclone-config ... :pg-conn-string ...}."
  [env-map]
  {:encryption-key (get env-map "BACKUP_ENCRYPTION_KEY")
   :rclone-config  (get env-map "RCLONE_CONFIG")
   :pg-conn-string (get env-map "PGCONNSTRING")})

(defn resolve-secrets
  "Read required secrets from environment variables and attach them to config.

   Required env vars: BACKUP_ENCRYPTION_KEY, RCLONE_CONFIG
   Optional env var:  PGCONNSTRING (nil when absent — a warning is logged)

   Returns config with :secrets map attached:
     {:encryption-key  \"...\"
      :rclone-config   \"...\"
      :pg-conn-string  \"...\" (or nil)}

   Throws ex-info with :missing-secrets listing ALL missing required secret
   names when any required secret is absent. Secret values are NEVER included
   in any log output or exception message.

   When (infisical-mode? config) is true, this instead resolves
   :pg-conn-string and :b2-rclone-env via resolve-db-credential!/
   resolve-b2-credential! (infisical-secrets spec, Requirements 2, 3, 5).
   BACKUP_ENCRYPTION_KEY remains required from the environment either way
   (Requirement 5.1). The legacy-only branch below is untouched — byte-for-
   byte identical to this function's behavior before that feature existed
   (Requirement 5.2)."
  [config]
  (if (infisical-mode? config)
    (let [encryption-key (getenv "BACKUP_ENCRYPTION_KEY")]
      (when (nil? encryption-key)
        (log/error "Missing required secrets (env vars):" ["BACKUP_ENCRYPTION_KEY"])
        (throw (ex-info "Missing required secrets"
                        {:missing-secrets ["BACKUP_ENCRYPTION_KEY"]})))
      (let [infisical-cfg  (:infisical config)
            rclone-config  (getenv "RCLONE_CONFIG")
            pg-conn-string (resolve-db-credential! {"PGCONNSTRING" (getenv "PGCONNSTRING")} infisical-cfg)
            b2-result      (resolve-b2-credential! {"RCLONE_CONFIG" rclone-config} infisical-cfg)]
        (assoc config :secrets
               {:encryption-key encryption-key
                :rclone-config  rclone-config
                :pg-conn-string pg-conn-string
                :b2-rclone-env  (if (= :infisical (:mode b2-result)) (:env b2-result) {})})))
    (let [env-map (read-secret-env-vars)
          missing (missing-required-secrets env-map)]
      (when (seq missing)
        (log/error "Missing required secrets (env vars):" missing)
        (throw (ex-info "Missing required secrets"
                        {:missing-secrets missing})))
      (when (nil? (get env-map "PGCONNSTRING"))
        (log/warn "PGCONNSTRING not set — PostgreSQL persistence will be skipped"))
      (assoc config :secrets (build-secrets-map env-map)))))
