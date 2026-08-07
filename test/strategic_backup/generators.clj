(ns strategic-backup.generators
  "Shared test.check generators for strategic-backup property tests."
  (:require [clojure.string :as str]
            [clojure.test.check.generators :as gen]))

;; ---------------------------------------------------------------------------
;; Primitive generators
;; ---------------------------------------------------------------------------

(def gen-lowercase-char
  "A single lowercase ASCII letter [a-z]."
  (gen/fmap char (gen/choose (int \a) (int \z))))

(def gen-lowercase-alnum-char
  "A single lowercase alphanumeric character [a-z0-9]."
  (gen/one-of [(gen/fmap char (gen/choose (int \a) (int \z)))
               (gen/fmap char (gen/choose (int \0) (int \9)))]))

;; ---------------------------------------------------------------------------
;; Named generators
;; ---------------------------------------------------------------------------

(def gen-dataset-name
  "Strings matching [a-z][a-z0-9]*/[a-z][a-z0-9]* e.g. \"tank/syncthing\"."
  (gen/let [pool-first gen-lowercase-char
            pool-rest  (gen/vector gen-lowercase-alnum-char 0 7)
            ds-first   gen-lowercase-char
            ds-rest    (gen/vector gen-lowercase-alnum-char 0 7)]
    (str (apply str pool-first pool-rest)
         "/"
         (apply str ds-first ds-rest))))

(def gen-snapshot-prefix
  "Non-empty alphanumeric strings e.g. \"backup\"."
  (gen/let [first-char gen-lowercase-char
            rest-chars (gen/vector gen-lowercase-alnum-char 0 11)]
    (apply str first-char rest-chars)))

(def gen-timestamp-str
  "Strings in YYYY-MM-DDTHHMMSS format (archive filename timestamp).
   e.g. \"2026-05-11T020000\""
  (gen/let [year   (gen/choose 2000 2099)
            month  (gen/choose 1 12)
            day    (gen/choose 1 28)
            hour   (gen/choose 0 23)
            minute (gen/choose 0 59)
            second (gen/choose 0 59)]
    (format "%04d-%02d-%02dT%02d%02d%02d"
            year month day hour minute second)))

(def gen-iso8601-str
  "Strings in YYYY-MM-DDTHH:MM:SSZ format (snapshot name / manifest timestamp).
   e.g. \"2026-05-11T02:00:00Z\""
  (gen/let [year   (gen/choose 2000 2099)
            month  (gen/choose 1 12)
            day    (gen/choose 1 28)
            hour   (gen/choose 0 23)
            minute (gen/choose 0 59)
            second (gen/choose 0 59)]
    (format "%04d-%02d-%02dT%02d:%02d:%02dZ"
            year month day hour minute second)))

(def gen-checksum
  "Strings matching \"sha256:[0-9a-f]{64}\"."
  (gen/let [hex-chars (gen/vector (gen/elements (vec "0123456789abcdef")) 64)]
    (str "sha256:" (apply str hex-chars))))

(def gen-filename-list
  "Lists of archive filenames with valid embedded timestamps.
   Filenames are of the form \"pool-dataset-YYYY-MM-DDTHHMMSS.zfs.gz\"
   or \"pool-dataset-YYYY-MM-DDTHHMMSS.zfs.gz.enc\"."
  (gen/list
   (gen/let [dataset   gen-dataset-name
             timestamp gen-timestamp-str
             encrypted gen/boolean]
     (let [slug (str/replace dataset "/" "-")
           ext  (if encrypted ".zfs.gz" ".zfs.gz.enc")]
       (str slug "-" timestamp ext)))))

(def gen-retention-count
  "Positive integers (1 or more)."
  (gen/fmap inc gen/nat))

(def gen-manifest
  "Maps with all required manifest fields."
  (gen/let [dataset         gen-dataset-name
            snapshot-prefix gen-snapshot-prefix
            timestamp       gen-iso8601-str
            stream-checksum gen-checksum
            created-at      gen-iso8601-str
            zfs-encrypted   gen/boolean
            compression     (gen/elements ["gzip"])
            archive-ts      gen-timestamp-str
            file-count      (gen/choose 0 5)
            file-paths      (gen/vector
                             (gen/let [name (gen/not-empty gen/string-alphanumeric)]
                               (str "./" name ".txt"))
                             file-count)
            checksums       (gen/vector gen-checksum file-count)]
    (let [slug      (str/replace dataset "/" "-")
          ext       (if zfs-encrypted ".zfs.gz" ".zfs.gz.enc")
          archive   (str slug "-" archive-ts ext)
          snapshot  (str dataset "@" snapshot-prefix "-" timestamp)
          files-map (zipmap file-paths checksums)]
      {:dataset         dataset
       :snapshot        snapshot
       :stream-checksum stream-checksum
       :created-at      created-at
       :zfs-encrypted   zfs-encrypted
       :compression     compression
       :archive-file    archive
       :files           files-map})))

(def gen-config
  "Maps with all required config fields."
  (gen/let [dataset                  gen-dataset-name
            test-dataset             gen-dataset-name
            b2-bucket                (gen/not-empty gen/string-alphanumeric)
            retention-count          gen-retention-count
            local-manifest-retention gen-retention-count
            compression              (gen/elements ["gzip"])
            encryption-cipher        (gen/elements ["aes-256-cbc"])
            snapshot-prefix          gen-snapshot-prefix]
    {:dataset                  dataset
     :test-dataset             test-dataset
     :staging-dir              "/var/backup/staging"
     :b2-bucket                b2-bucket
     :b2-path-prefix           "zfs-backups/"
     :retention-count          retention-count
     :local-manifest-retention local-manifest-retention
     :compression              compression
     :encryption-cipher        encryption-cipher
     :snapshot-prefix          snapshot-prefix}))
