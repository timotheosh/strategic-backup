(ns strategic-backup.pipeline
  (:require [clojure.string :as str]))

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