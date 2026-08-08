(ns strategic-backup.infisical
  "Optional secret retrieval from Infisical (Requirements 2-4 of the
   infisical-secrets spec). The sole namespace that calls clj-infisical —
   everywhere else, whether to call it at all is a pure decision made in
   strategic-backup.config."
  (:require [clj-infisical.core :as infisical-core]))

(defn b2-rclone-env-vars
  "Calculation. Turns a B2 Key ID and Application Key into the three
   RCLONE_CONFIG_B2_* environment variables rclone needs to authenticate
   against B2 directly, with no rclone.conf file (Req 3.3)."
  [key-id application-key]
  {"RCLONE_CONFIG_B2_TYPE"    "b2"
   "RCLONE_CONFIG_B2_ACCOUNT" key-id
   "RCLONE_CONFIG_B2_KEY"     application-key})

(defn fetch-secret!
  "Action. Fetches `secret-name` from Infisical using `infisical-config`
   (the :project-id/:environment/:secret-path/:site-url map — Req 1.2).

   Client authentication is never passed explicitly; clj-infisical resolves
   its own Universal Auth credentials from INFISICAL_CLIENT_ID/SECRET or
   /etc/infisical files (Req 4.1).

   Returns the secret's plaintext value on success.
   On any clj-infisical failure (auth, not-found, network, etc.), throws
   ex-info with :stage :secrets, :secret-name, and :infisical-error (the
   underlying :clj-infisical/... type keyword) — never the secret value
   (Req 2.3, 3.4, 4.2)."
  [infisical-config secret-name]
  (try
    (infisical-core/get-secret! (assoc infisical-config :secret-name secret-name))
    (catch clojure.lang.ExceptionInfo e
      (throw (ex-info (str "Infisical secret fetch failed for " secret-name)
                      {:stage          :secrets
                       :secret-name    secret-name
                       :infisical-error (:type (ex-data e))}
                      e)))))
