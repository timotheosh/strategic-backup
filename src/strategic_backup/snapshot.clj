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

(defn create-snapshot!
  "Runs `zfs snapshot <snapshot-name>` via the executor.
   Returns snapshot-name on success; throws ex-info with :stage :snapshot on non-zero exit."
  [executor snap-name]
  (let [result (shell/run-cmd executor "zfs" ["snapshot" snap-name] {})]
    (when (not= 0 (:exit result))
      (throw (ex-info "zfs snapshot failed"
                      {:stage :snapshot
                       :cmd   (:cmd result)
                       :exit  (:exit result)
                       :err   (:err result)})))
    snap-name))

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
  (let [result (shell/run-cmd executor "zfs" ["receive" test-dataset] {})]
    (when (not= 0 (:exit result))
      (throw (ex-info "zfs receive failed"
                      {:stage :receive
                       :cmd   (:cmd result)
                       :exit  (:exit result)
                       :err   (:err result)})))
    test-dataset))

(defn destroy-dataset!
  "Runs `zfs destroy -r <dataset>` via the executor.
   Always returns nil regardless of exit code (cleanup must not throw)."
  [executor dataset]
  (shell/run-cmd executor "zfs" ["destroy" "-r" dataset] {})
  nil)
