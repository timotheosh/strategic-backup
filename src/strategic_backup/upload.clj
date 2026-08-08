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

(defn rclone-copy!
  "Action. Uploads `local-path` to `remote` via `rclone copy` — never
   `rclone sync`, so existing remote objects are never deleted or
   overwritten (Req 4.2).
   Returns nil on success; throws ex-info with :stage :upload on failure."
  [executor local-path remote]
  (let [result (shell/run-cmd executor "rclone" ["copy" local-path remote] {})]
    (when (not= 0 (:exit result))
      (throw (ex-info "rclone copy failed"
                      {:stage :upload
                       :cmd   (:cmd result)
                       :exit  (:exit result)
                       :err   (:err result)})))
    nil))

(defn download-archive!
  "Action. Downloads `filename` from `remote` into `local-dir` via
   `rclone copy`.
   Returns nil on success; throws ex-info with :stage :download on failure."
  [executor remote filename local-dir]
  (let [source (str remote "/" filename)
        result (shell/run-cmd executor "rclone" ["copy" source local-dir] {})]
    (when (not= 0 (:exit result))
      (throw (ex-info "rclone download failed"
                      {:stage :download
                       :cmd   (:cmd result)
                       :exit  (:exit result)
                       :err   (:err result)})))
    nil))

(defn delete-remote!
  "Action. Deletes `filename` from `remote` via `rclone delete`.
   Never throws — returns {:ok true} on success or {:ok false :error
   \"...\"} on failure, so retention enforcement can continue past
   individual failures (Req 5.5)."
  [executor remote filename]
  (let [target (str remote "/" filename)
        result (shell/run-cmd executor "rclone" ["delete" target] {})]
    (if (= 0 (:exit result))
      {:ok true}
      {:ok false :error (:err result)})))

(defn list-remote
  "Action. Lists archive filenames present at `remote` via `rclone lsf`.
   Returns an empty seq (rather than throwing) when the command fails."
  [executor remote]
  (let [result (shell/run-cmd executor "rclone" ["lsf" remote] {})]
    (if (= 0 (:exit result))
      (parse-lsf-output (:out result))
      [])))
