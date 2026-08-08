(ns strategic-backup.config
  "Configuration loading, validation, and secret resolution.

   Config is loaded from an EDN file whose path is given by the
   STRATEGIC_BACKUP_CONFIG environment variable, defaulting to
   /etc/strategic-backup/config.edn.

   Secrets are read exclusively from environment variables and are
   never logged or included in exception messages."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
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
  config)

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
   in any log output or exception message."
  [config]
  (let [env-map (read-secret-env-vars)
        missing (missing-required-secrets env-map)]
    (when (seq missing)
      (log/error "Missing required secrets (env vars):" missing)
      (throw (ex-info "Missing required secrets"
                      {:missing-secrets missing})))
    (when (nil? (get env-map "PGCONNSTRING"))
      (log/warn "PGCONNSTRING not set — PostgreSQL persistence will be skipped"))
    (assoc config :secrets (build-secrets-map env-map))))
