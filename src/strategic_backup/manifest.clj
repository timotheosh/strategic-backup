(ns strategic-backup.manifest
  "Manifest generation, checksums, and local persistence (Requirement 2)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [strategic-backup.retention :as retention]
            [strategic-backup.shell :as shell]
            [taoensso.timbre :as log]))

(defn dataset-mountpoint
  "Calculation. Derives the local filesystem mountpoint to checksum for a
   given ZFS dataset name.

   NOTE: this replicates the pre-existing inline logic from core.clj
   verbatim — `(str \"/\" (str/replace dataset \"/\" \"/\"))` — which
   replaces every '/' in the dataset name with '/', a no-op, so the result
   is simply \"/\" + the dataset name unchanged (e.g. \"tank/syncthing\" ->
   \"/tank/syncthing\"). This looks like it may have been intended to do
   something else; preserved exactly as-is here since fixing it would
   change observable behavior — flagged separately as a decision for you."
  [dataset]
  (str "/" (str/replace dataset "/" "/")))

(defn parse-openssl-checksum
  "Calculation. Parses `openssl dgst -sha256` output (e.g.
   \"SHA256(/path/to/file)= <hex>\\n\") into the manifest's checksum format
   \"sha256:<hex>\". Shared with strategic-backup.verify — same parsing
   need, not duplicated."
  [output]
  (when-let [hex (second (re-find #"=\s*([0-9a-fA-F]{64})" output))]
    (str "sha256:" hex)))

(defn relativize-path
  "Calculation. Converts an absolute file path under `mountpoint` into the
   \"./relative/path\" form used as keys in the manifest's :files map."
  [mountpoint absolute-path]
  (let [prefix (if (str/ends-with? mountpoint "/") mountpoint (str mountpoint "/"))]
    (str "./" (str/replace-first absolute-path prefix ""))))

(defn manifest-filename
  "Calculation. Local manifest filename for a given archive filename:
   \"<archive-file>.manifest.edn\".

   Req 2.4 describes this as \"<snapshot-name>.manifest.edn\", but the
   snapshot name embeds the raw dataset name (e.g. \"tank/syncthing@...\"),
   which contains '/' and can't be used literally as a filename. The
   already slug-safe archive filename is used instead — consistent with
   how core_test.clj's own write-edn! stand-ins model this file's naming."
  [archive-file]
  (str archive-file ".manifest.edn"))

(defn build-manifest
  "Calculation. Assembles the manifest map with exactly the field set
   required by Req 2.4."
  [dataset snapshot stream-checksum created-at zfs-encrypted? compression
   archive-file file-checksums]
  {:dataset         dataset
   :snapshot        snapshot
   :stream-checksum stream-checksum
   :created-at      created-at
   :zfs-encrypted   zfs-encrypted?
   :compression     compression
   :archive-file    archive-file
   :files           file-checksums})

(defn format-hex
  "Calculation. Converts a byte array into a lowercase hex string."
  [^bytes bytes]
  (apply str (map #(format "%02x" (bit-and (int %) 0xff)) bytes)))

(defn hash-file
  "Action. Computes the SHA-256 hex digest of a single file's contents via
   streaming buffered reads through java.security.MessageDigest — no
   subprocess spawn (pipeline-timing spec, Req 1.1). Returns
   \"sha256:<hex>\".

   Throws on any read failure (permission denied, file vanished, ...) —
   never catches. compute-file-checksums owns the decision of what to do
   with a failure (exclude-and-continue vs. abort), not this function."
  [^java.io.File file]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (with-open [in (io/input-stream file)]
      (let [buf (byte-array 8192)]
        (loop []
          (let [n (.read in buf)]
            (when (pos? n)
              (.update digest buf 0 n)
              (recur))))))
    (str "sha256:" (format-hex (.digest digest)))))

(defn compute-file-checksums
  "Action. Walks every regular file under `mountpoint` and computes its
   SHA-256 checksum in-process via hash-file — no subprocess spawn per
   file (pipeline-timing spec, Req 1.1; previously shelled out to
   `openssl dgst -sha256` once per file, which dominated wall-clock time
   on datasets with many small files). Returns a map of relative path
   (\"./...\") -> \"sha256:<hex>\".

   `strict?` false (default posture — see core.clj/restore.clj callers):
   a file that fails to hash is logged via log/warn and EXCLUDED from the
   returned map — never given a nil/bogus checksum (Req 1.2/1.3). The
   backup proceeds regardless.

   `strict?` true: a hash failure is wrapped in ex-info {:stage :checksum
   :path abs-path} and propagates immediately, aborting the caller
   (Req 1.4) — enabled via the --strict-checksums CLI flag."
  [mountpoint strict?]
  (let [root  (io/file mountpoint)
        files (->> (file-seq root)
                   (filter #(.isFile ^java.io.File %)))]
    (into {}
          (keep (fn [^java.io.File f]
                  (let [abs-path (.getAbsolutePath f)]
                    (try
                      [(relativize-path mountpoint abs-path) (hash-file f)]
                      (catch Exception e
                        (if strict?
                          (throw (ex-info "Failed to checksum file"
                                          {:stage :checksum :path abs-path}
                                          e))
                          (do
                            (log/warn "Failed to checksum file — excluded from manifest"
                                      {:path abs-path :error (.getMessage e)})
                            nil)))))))
          files)))

(defn compute-stream-checksum
  "Action. Computes the SHA-256 checksum of the final archive file via
   `openssl dgst -sha256` (Req 2.3)."
  [executor archive-path]
  (let [result (shell/run-cmd executor "openssl" ["dgst" "-sha256" archive-path] {})]
    (parse-openssl-checksum (:out result))))

(defn write-edn!
  "Action. Writes `manifest` as EDN to `staging-dir`, named per
   `manifest-filename`. Returns the full path written (Req 2.4)."
  [manifest staging-dir]
  (let [path (str staging-dir "/" (manifest-filename (:archive-file manifest)))]
    (spit path (pr-str manifest))
    path))

(defn- sorted-local-edn-filenames
  "Action (private). Lists *.manifest.edn filenames present in
   `staging-dir`, sorted oldest-first by embedded timestamp. Shared by
   `prune-local-edns!` and `latest-local-edn` so the listing/filtering
   logic isn't duplicated."
  [staging-dir]
  (let [dir (io/file staging-dir)]
    (->> (.listFiles dir)
         (filter #(str/ends-with? (.getName ^java.io.File %) ".manifest.edn"))
         (map #(.getName ^java.io.File %))
         retention/sort-by-timestamp)))

(defn prune-local-edns!
  "Action. Deletes old local *.manifest.edn files in `staging-dir` beyond
   `retention-count` (Req 12.3), reusing the same keep-newest-N business
   rule as remote retention — retention/select-for-deletion (Req 5).
   Returns the count of files deleted."
  [staging-dir retention-count]
  (let [sorted    (sorted-local-edn-filenames staging-dir)
        to-delete (retention/select-for-deletion sorted retention-count)]
    (doseq [filename to-delete]
      (.delete (io/file staging-dir filename)))
    (count to-delete)))

(defn latest-local-edn
  "Action (Req 7.1, Mode 2). Reads the most recent local *.manifest.edn
   file in `staging-dir` (by embedded timestamp), parsed back into a
   manifest map.

   Returns nil — rather than throwing — when the staging dir has no
   manifest files, or the newest one fails to parse; callers fall back to
   the next tier (ultimately Mode 3) rather than aborting (Req 8.4)."
  [staging-dir]
  (when-let [newest (last (sorted-local-edn-filenames staging-dir))]
    (try
      (edn/read-string (slurp (io/file staging-dir newest)))
      (catch Exception e
        (log/warn "Failed to parse local manifest EDN:" (.getMessage e))
        nil))))
