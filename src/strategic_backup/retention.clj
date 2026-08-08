(ns strategic-backup.retention
  "Remote retention policy enforcement (Requirement 5)."
  (:require [strategic-backup.upload :as upload]))

(defn parse-timestamp-from-filename
  "Calculation. Extracts the embedded \"YYYY-MM-DDTHHMMSS\" timestamp from
   a filename (e.g. \"tank-syncthing-2026-05-11T020000.zfs.gz.enc\" or its
   local manifest counterpart \"tank-syncthing-2026-05-11T020000.zfs.gz.enc.manifest.edn\"
   -> \"2026-05-11T020000\"), or nil if no such timestamp is present.
   The timestamp is parsed directly from the filename — never from B2
   object metadata or a manifest (Req 5.1). Not anchored to a specific
   suffix so the same rule applies to remote archive filenames (Req 5) and
   local EDN manifest filenames (Req 12.3), which share the same
   `<archive-file>[.manifest.edn]` naming scheme — see manifest/manifest-filename."
  [filename]
  (second (re-find #"(\d{4}-\d{2}-\d{2}T\d{6})" filename)))

(defn sort-by-timestamp
  "Calculation. Sorts archive filenames ascending (oldest first) by their
   embedded timestamp (Req 5.1). The zero-padded YYYY-MM-DDTHHMMSS format
   sorts correctly as a plain string, so no date parsing is required."
  [filenames]
  (sort-by parse-timestamp-from-filename filenames))

(defn select-for-deletion
  "Calculation — the retention business rule (Req 5.2). Given `sorted-filenames`
   already sorted oldest-first and a positive `retention-count`, returns the
   oldest filenames that exceed the retention count, i.e. the ones to delete.
   Returns an empty seq when there are retention-count or fewer filenames."
  [sorted-filenames retention-count]
  (let [excess (- (count sorted-filenames) retention-count)]
    (if (pos? excess)
      (take excess sorted-filenames)
      [])))

(defn enforce-retention!
  "Action. Applies the retention policy to `remote`: deletes the archives
   selected by `select-for-deletion`, continuing past individual deletion
   failures rather than aborting (Req 5.5).

   `env` (infisical-secrets spec, Requirement 3.3) is forwarded to every
   `upload/delete-remote!` call — e.g. the RCLONE_CONFIG_B2_* vars when B2
   credentials came from Infisical. Omitting it (or passing {}) reproduces
   today's behavior exactly.

   Returns {:deleted [filename ...] :failed [{:filename .. :error ..} ...]}."
  ([executor remote sorted-filenames retention-count]
   (enforce-retention! executor remote sorted-filenames retention-count {}))
  ([executor remote sorted-filenames retention-count env]
   (let [to-delete (select-for-deletion sorted-filenames retention-count)]
     (reduce
      (fn [acc filename]
        (let [result (upload/delete-remote! executor remote filename env)]
          (if (:ok result)
            (update acc :deleted conj filename)
            (update acc :failed conj {:filename filename :error (:error result)}))))
      {:deleted [] :failed []}
      to-delete))))
