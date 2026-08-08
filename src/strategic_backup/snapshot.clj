(ns strategic-backup.snapshot
  (:require [clojure.string :as str]
            [strategic-backup.shell :as shell]))

(defn snapshot-name
  "Pure function. Returns \"<dataset>@<prefix>-<timestamp>\"."
  [dataset prefix timestamp]
  (str dataset "@" prefix "-" timestamp))

(defn send-stream-cmd
  "Pure function. Returns the zfs send -w command string for the given snapshot."
  [snapshot-name]
  (str "zfs send -w " snapshot-name))

(defn- run-zfs-cmd!
  "Action (private). Runs `zfs <args>` via the executor.
   Returns `result-value` on success (exit 0); throws ex-info tagged with
   `stage` (plus :cmd/:exit/:err) and `error-message` on non-zero exit.
   Shared by create-snapshot! and receive-stream!, which differ only in
   the subcommand, stage tag, error message, and success return value."
  [executor args stage error-message result-value]
  (let [result (shell/run-cmd executor "zfs" args {})]
    (when (not= 0 (:exit result))
      (throw (ex-info error-message
                      {:stage stage
                       :cmd   (:cmd result)
                       :exit  (:exit result)
                       :err   (:err result)})))
    result-value))

(defn create-snapshot!
  "Runs `zfs snapshot <snapshot-name>` via the executor.
   Returns snapshot-name on success; throws ex-info with :stage :snapshot on non-zero exit."
  [executor snap-name]
  (run-zfs-cmd! executor ["snapshot" snap-name] :snapshot "zfs snapshot failed" snap-name))

(defn encrypted-value?
  "Calculation. Returns true unless the trimmed `zfs get encryption` output is \"off\"."
  [output]
  (not= "off" (str/trim output)))

(defn zfs-encrypted?
  "Runs `zfs get -H -o value encryption <dataset>` and returns true unless the value is \"off\"."
  [executor dataset]
  (let [result (shell/run-cmd executor "zfs" ["get" "-H" "-o" "value" "encryption" dataset] {})]
    (encrypted-value? (:out result))))

(defn receive-stream!
  "Runs `zfs receive <test-dataset>` via the executor.
   Returns test-dataset on success; throws ex-info with :stage :receive on non-zero exit."
  [executor test-dataset]
  (run-zfs-cmd! executor ["receive" test-dataset] :receive "zfs receive failed" test-dataset))

(defn destroy-dataset!
  "Runs `zfs destroy -r <dataset>` via the executor.
   Always returns nil regardless of exit code (cleanup must not throw)."
  [executor dataset]
  (shell/run-cmd executor "zfs" ["destroy" "-r" dataset] {})
  nil)
