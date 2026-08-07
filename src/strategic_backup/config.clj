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

(defn load-config
  "Load the EDN config map from `path`.

   If `path` is nil, the path is resolved from the STRATEGIC_BACKUP_CONFIG
   environment variable, falling back to /etc/strategic-backup/config.edn.

   Throws ex-info with:
     :reason :file-not-found  — when the file does not exist
     :reason :parse-error     — when the file content is not valid EDN"
  [path]
  (let [resolved-path (or path
                          (System/getenv "STRATEGIC_BACKUP_CONFIG")
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

(defn validate-config
  "Validate that `config` contains all required keys.

   Returns `config` unchanged when valid.

   Throws ex-info with :missing-keys containing a vector of ALL absent
   required keys (not just the first one encountered)."
  [config]
  (let [missing (filterv #(not (contains? config %)) required-config-keys)]
    (when (seq missing)
      (log/error "Config validation failed. Missing keys:" missing)
      (throw (ex-info "Config is missing required keys"
                      {:missing-keys missing}))))
  config)

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
  (let [missing-required (filterv #(nil? (getenv %)) required-secret-env-vars)]
    (when (seq missing-required)
      (log/error "Missing required secrets (env vars):" missing-required)
      (throw (ex-info "Missing required secrets"
                      {:missing-secrets missing-required}))))
  (let [pg-conn-string (getenv "PGCONNSTRING")]
    (when (nil? pg-conn-string)
      (log/warn "PGCONNSTRING not set — PostgreSQL persistence will be skipped"))
    (assoc config :secrets
           {:encryption-key  (getenv "BACKUP_ENCRYPTION_KEY")
            :rclone-config   (getenv "RCLONE_CONFIG")
            :pg-conn-string  pg-conn-string})))
