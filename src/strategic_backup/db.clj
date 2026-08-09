(ns strategic-backup.db
  "Manifest persistence to PostgreSQL (Requirement 12) — a convenience
   index only. Per the requirements doc's Core Principle, the backup and
   restore pipeline must remain fully operational without PostgreSQL; this
   namespace never blocks or aborts a backup on its own failure — see how
   core.clj calls persist-manifest! inside a fire-and-forget future/try."
  (:require [clojure.data.json :as json]
            [next.jdbc :as jdbc]
            [taoensso.timbre :as log]))

(defn manifest->json
  "Calculation. Converts a manifest map to a JSON string via
   clojure.data.json (Req 12.1)."
  [manifest]
  (json/write-str manifest))

(defn json->manifest
  "Calculation. Inverse of `manifest->json`. Parses a JSON string back into
   a manifest map with keyword top-level keys.

   JSON object keys are always strings, so a plain `:key-fn keyword` parse
   would also turn the :files map's relative-path keys into keywords (e.g.
   \"./a.txt\" -> :./a.txt) — this restores :files to its original
   string-keyed form afterward, since `(name (keyword s))` recovers `s`
   exactly."
  [json-str]
  (let [parsed (json/read-str json-str :key-fn keyword)]
    (update parsed :files
            (fn [files] (into {} (map (fn [[k v]] [(name k) v]) files))))))

(defn persist-manifest!
  "Action. Inserts `manifest` (as JSONB) into the backup_manifests table
   over `pg-conn-string`.

   When `pg-conn-string` is nil, logs that PostgreSQL is unavailable and
   returns without attempting any database operation (Req 12.5) — checked
   here, up front, rather than discovering it via a failed connection
   attempt.

   Returns {:ok true} on success. Throws on failure; the caller treats
   persistence as fire-and-forget and never aborts the backup because of
   it (Req 12.2)."
  [pg-conn-string manifest]
  (if (nil? pg-conn-string)
    (do (log/warn "No PostgreSQL connection string — skipping manifest persistence")
        {:ok false :reason :no-connection-string})
    (let [ds (jdbc/get-datasource pg-conn-string)]
      (jdbc/execute! ds
                     ["INSERT INTO backup_manifests (snapshot, manifest) VALUES (?, ?::jsonb)"
                      (:snapshot manifest)
                      (manifest->json manifest)])
      {:ok true})))

(defn- root-cause
  "Calculation. Walks `.getCause` until there is no further cause,
   returning `e` itself when it has none."
  [e]
  (loop [current e]
    (if-let [cause (ex-cause current)]
      (recur cause)
      current)))

(defn- exception-detail
  "Calculation. `e`'s own message, plus the root cause's message when it
   differs — pgjdbc wraps low-level connection failures (DNS, connection
   refused, timeout, TLS) in a generic top-level message (e.g. \"The
   connection attempt failed.\") whose own .getMessage hides the actually
   useful detail, which lives on the root cause instead."
  [e]
  (let [top      (.getMessage e)
        root     (root-cause e)
        root-msg (when-not (identical? root e) (.getMessage root))]
    (if (and root-msg (not= root-msg top))
      (str top " — caused by: " root-msg)
      top)))

(defn test-connection!
  "Action (--db-test CLI flag). Attempts to connect to `pg-conn-string` and
   run a trivial query (`SELECT 1`).

   Never throws — returns {:ok true} on success or {:ok false :error
   \"...\"} on any failure, including `pg-conn-string` being nil (no
   PGCONNSTRING configured via env or Infisical). Unlike
   persist-manifest!/fetch-latest-manifest, this is meant to be checked
   and acted on directly by the caller (exit code), not fired-and-forgotten."
  [pg-conn-string]
  (if (nil? pg-conn-string)
    {:ok false :error "No PGCONNSTRING configured (via environment or Infisical)"}
    (try
      (let [ds (jdbc/get-datasource pg-conn-string)]
        (jdbc/execute! ds ["SELECT 1"])
        {:ok true})
      (catch Exception e
        {:ok false :error (exception-detail e)}))))

(defn fetch-latest-manifest
  "Action (Req 7.1, Mode 1). Queries PostgreSQL for the most recent
   manifest record whose snapshot belongs to `dataset`, parsed back into a
   manifest map.

   Returns nil — rather than throwing — when `pg-conn-string` is nil, or
   the query fails, or finds nothing. PostgreSQL is a convenience index
   only (see the Core Principle in the requirements doc); callers fall
   back to a local EDN manifest or Mode 3 rather than aborting."
  [pg-conn-string dataset]
  (when (some? pg-conn-string)
    (try
      (let [ds  (jdbc/get-datasource pg-conn-string)
            row (jdbc/execute-one!
                 ds
                 ["SELECT manifest FROM backup_manifests WHERE snapshot LIKE ? ORDER BY created_at DESC LIMIT 1"
                  (str dataset "@%")])]
        (when-let [json-str (:backup_manifests/manifest row)]
          (json->manifest json-str)))
      (catch Exception e
        (log/warn "Failed to fetch manifest from PostgreSQL:" (.getMessage e))
        nil))))
