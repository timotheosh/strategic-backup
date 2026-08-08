(ns strategic-backup.pipeline
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [strategic-backup.shell :as shell]))

(defn dataset-slug
  "Replace all '/' characters in a ZFS dataset name with '-'.
   E.g. \"tank/syncthing\" => \"tank-syncthing\""
  [dataset]
  (str/replace dataset "/" "-"))

(defn archive-filename
  "Return the archive filename for the given dataset slug, timestamp, and
   zfs-encrypted? flag.

   When zfs-encrypted? is true  => \"<slug>-<timestamp>.zfs.gz\"
   When zfs-encrypted? is false => \"<slug>-<timestamp>.zfs.gz.enc\""
  [dataset-slug timestamp zfs-encrypted?]
  (let [ext (if zfs-encrypted? ".zfs.gz" ".zfs.gz.enc")]
    (str dataset-slug "-" timestamp ext)))

(defn compact-timestamp
  "Calculation. Converts an ISO-8601 instant string (e.g. the output of
   `(.toString (Instant/now))`, which includes fractional seconds and a
   trailing Z) into the compact archive-filename timestamp format
   \"YYYY-MM-DDTHHMMSS\" expected by `archive-filename` — the whole-seconds
   prefix with colons stripped."
  [iso-string]
  (str/replace (subs iso-string 0 19) ":" ""))

;; ---------------------------------------------------------------------------
;; Pipeline stage command fragments (Requirement 3)
;; ---------------------------------------------------------------------------

(defn compress-cmd
  "Calculation. Shell command fragment for compressing stdin to stdout with
   the given compression algorithm identifier."
  [compression]
  (case compression
    "gzip" "gzip -c"
    (throw (ex-info "Unsupported compression algorithm"
                    {:stage :pipeline :compression compression}))))

(defn decompress-cmd
  "Calculation. Shell command fragment for decompressing stdin to stdout
   with the given compression algorithm identifier (inverse of `compress-cmd`,
   used during restore)."
  [compression]
  (case compression
    "gzip" "gzip -dc"
    (throw (ex-info "Unsupported compression algorithm"
                    {:stage :pipeline :compression compression}))))

(defn encrypt-cmd
  "Calculation. Shell command fragment for encrypting stdin to stdout with
   `openssl enc`. The key is read from the BACKUP_ENCRYPTION_KEY environment
   variable via `-pass env:...` — never as a literal argument — so it never
   appears in the process list (Req 3.5)."
  [cipher]
  (str "openssl enc -" cipher " -salt -pass env:BACKUP_ENCRYPTION_KEY"))

(defn decrypt-cmd
  "Calculation. Shell command fragment for decrypting stdin to stdout with
   `openssl enc -d` (inverse of `encrypt-cmd`, used during restore)."
  [cipher]
  (str "openssl enc -d -" cipher " -pass env:BACKUP_ENCRYPTION_KEY"))

(defn build-pipeline-cmd
  "Calculation. Composes the full backup shell pipeline string:
     <send-cmd> | <compress-cmd> [| <encrypt-cmd>] > <archive-path>
   The encryption stage is omitted when zfs-encrypted? is true (Req 3.4) —
   the source dataset's own ZFS-native encryption already protects the
   stream, so no additional openssl enc layer is applied (Req 3.3)."
  [send-cmd compression zfs-encrypted? cipher archive-path]
  (let [stages (cond-> [send-cmd (compress-cmd compression)]
                 (not zfs-encrypted?) (conj (encrypt-cmd cipher)))]
    (str (str/join " | " stages) " > " archive-path)))

(defn build-restore-pipeline-cmd
  "Calculation. Composes the full restore shell pipeline string:
     cat <archive-path> [| <decrypt-cmd>] | <decompress-cmd> | zfs receive <test-dataset>
   The decryption stage is included only when decrypt? is true (Req 7.7-7.9),
   mirroring `build-pipeline-cmd`'s encryption-stage logic in reverse."
  [archive-path compression decrypt? cipher test-dataset]
  (let [stages (cond-> [(str "cat " archive-path)]
                 decrypt? (conj (decrypt-cmd cipher))
                 true     (conj (decompress-cmd compression))
                 true     (conj (str "zfs receive " test-dataset)))]
    (str/join " | " stages)))

(defn extract-output-path
  "Calculation. Extracts the redirected output file path (the token after
   the final `>`) from a pipeline command string, or nil if none is found."
  [pipeline-cmd]
  (second (re-find #">\s*(\S+)" pipeline-cmd)))

(defn run-pipeline!
  "Executes `pipeline-cmd` (a full shell pipeline string ending in
   `> <archive-path>`) via the executor.

   Returns nil on success (exit 0).
   On non-zero exit, deletes any partial output file (Req 3.6) and throws
   ex-info with :stage :pipeline."
  [executor pipeline-cmd]
  (let [result (shell/run-pipeline executor pipeline-cmd)]
    (when (not= 0 (:exit result))
      (when-let [out-path (extract-output-path pipeline-cmd)]
        (let [f (io/file out-path)]
          (when (.exists f) (.delete f))))
      (throw (ex-info "Backup pipeline failed"
                      {:stage :pipeline
                       :cmd   (:cmd result)
                       :exit  (:exit result)
                       :err   (:err result)})))
    nil))