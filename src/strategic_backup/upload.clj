(ns strategic-backup.upload
  "Remote (Backblaze B2 via rclone) upload/download/list/delete (Requirement 4)."
  (:require [clojure.string :as str]
            [strategic-backup.shell :as shell]))

(defn remote-target
  "Calculation. Builds the rclone remote target spec \"b2:<bucket>/<prefix>\"
   from a B2 bucket name and path prefix."
  [bucket prefix]
  (str "b2:" bucket "/" prefix))

(defn parse-lsf-output
  "Calculation. Splits raw `rclone lsf` stdout into a seq of filenames,
   dropping blank lines."
  [raw-output]
  (->> (str/split-lines raw-output)
       (remove str/blank?)))

(defn ensure-b2-credentials-present!
  "Action. Throws ex-info {:stage :secrets} when neither an RCLONE_CONFIG
   file path nor Infisical-derived B2 env vars are available. B2 access is
   required for backup/restore-test to function at all, but this is
   checked here — right before B2 is actually touched — rather than
   unconditionally at config-resolution time, so a subcommand that never
   needs B2 (e.g. --db-test) isn't blocked by a missing B2 credential.
   config/resolve-b2-credential! never throws for exactly this reason;
   this is where B2's requiredness is actually enforced."
  [rclone-config b2-rclone-env]
  (when (and (nil? rclone-config) (empty? b2-rclone-env))
    (throw (ex-info "Unable to resolve B2 credential — no RCLONE_CONFIG and no Infisical B2 credentials configured"
                    {:stage :secrets})))
  nil)

(defn rclone-copy!
  "Action. Uploads `local-path` to `remote` via `rclone copy` — never
   `rclone sync`, so existing remote objects are never deleted or
   overwritten (Req 4.2).

   `env` (infisical-secrets spec, Requirement 3.3) is merged into the
   subprocess environment — e.g. the RCLONE_CONFIG_B2_* vars when B2
   credentials came from Infisical. Omitting it (or passing {}) reproduces
   today's behavior exactly: the subprocess just inherits the JVM's own
   environment, and rclone falls back to its own RCLONE_CONFIG-file
   resolution.

   Returns nil on success; throws ex-info with :stage :upload on failure."
  ([executor local-path remote] (rclone-copy! executor local-path remote {}))
  ([executor local-path remote env]
   (let [result (shell/run-cmd executor "rclone" ["copy" local-path remote] {:env env})]
     (when (not= 0 (:exit result))
       (throw (ex-info "rclone copy failed"
                       {:stage :upload
                        :cmd   (:cmd result)
                        :exit  (:exit result)
                        :err   (:err result)})))
     nil)))

(defn download-archive!
  "Action. Downloads `filename` from `remote` into `local-dir` via
   `rclone copy`. See `rclone-copy!` for `env` (Requirement 3.3).
   Returns nil on success; throws ex-info with :stage :download on failure."
  ([executor remote filename local-dir] (download-archive! executor remote filename local-dir {}))
  ([executor remote filename local-dir env]
   (let [source (str remote "/" filename)
         result (shell/run-cmd executor "rclone" ["copy" source local-dir] {:env env})]
     (when (not= 0 (:exit result))
       (throw (ex-info "rclone download failed"
                       {:stage :download
                        :cmd   (:cmd result)
                        :exit  (:exit result)
                        :err   (:err result)})))
     nil)))

(defn delete-remote!
  "Action. Deletes `filename` from `remote` via `rclone delete`. See
   `rclone-copy!` for `env` (Requirement 3.3).
   Never throws — returns {:ok true} on success or {:ok false :error
   \"...\"} on failure, so retention enforcement can continue past
   individual failures (Req 5.5)."
  ([executor remote filename] (delete-remote! executor remote filename {}))
  ([executor remote filename env]
   (let [target (str remote "/" filename)
         result (shell/run-cmd executor "rclone" ["delete" target] {:env env})]
     (if (= 0 (:exit result))
       {:ok true}
       {:ok false :error (:err result)}))))

(defn- rclone-lsf!
  "Action (private). Runs `rclone lsf <remote>` and returns the raw shell
   result map. Shared by `list-remote` (parses the listing) and
   `test-connection!` (only cares about success/failure) so the shell
   call itself isn't duplicated between them."
  [executor remote env]
  (shell/run-cmd executor "rclone" ["lsf" remote] {:env env}))

(defn list-remote
  "Action. Lists archive filenames present at `remote` via `rclone lsf`.
   See `rclone-copy!` for `env` (Requirement 3.3).
   Returns an empty seq (rather than throwing) when the command fails."
  ([executor remote] (list-remote executor remote {}))
  ([executor remote env]
   (let [result (rclone-lsf! executor remote env)]
     (if (= 0 (:exit result))
       (parse-lsf-output (:out result))
       []))))

(defn test-connection!
  "Action (--b2-test CLI flag). Attempts `rclone lsf <remote>` as a
   lightweight connectivity/credentials check — exercises B2 auth, bucket
   access, and network reachability without uploading, downloading, or
   deleting anything. See `rclone-copy!` for `env` (Requirement 3.3).

   Never throws — returns {:ok true} on success (exit 0, even for an
   empty bucket) or {:ok false :error \"...\"} on failure."
  ([executor remote] (test-connection! executor remote {}))
  ([executor remote env]
   (let [result (rclone-lsf! executor remote env)]
     (if (= 0 (:exit result))
       {:ok true}
       {:ok false :error (:err result)}))))
