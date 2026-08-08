(ns strategic-backup.db-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [next.jdbc :as jdbc]
            [strategic-backup.db :as db]))

;; ---------------------------------------------------------------------------
;; manifest->json
;; ---------------------------------------------------------------------------

(deftest manifest-to-json-round-trips-via-data-json
  (testing "produces a JSON string parseable back via clojure.data.json"
    (let [m      {:dataset  "tank/syncthing"
                  :snapshot "tank/syncthing@backup-2026-05-11T02:00:00Z"
                  :files    {"./a.txt" "sha256:abc"}}
          json-s (db/manifest->json m)
          parsed (json/read-str json-s :key-fn keyword)]
      (is (string? json-s))
      (is (= "tank/syncthing" (:dataset parsed)))
      (is (= "tank/syncthing@backup-2026-05-11T02:00:00Z" (:snapshot parsed))))))

;; ---------------------------------------------------------------------------
;; persist-manifest!
;; ---------------------------------------------------------------------------

(deftest persist-manifest-skips-when-conn-string-nil
  (testing "returns without attempting a DB connection when pg-conn-string is nil"
    (let [get-datasource-called (atom false)]
      (with-redefs [jdbc/get-datasource (fn [_] (reset! get-datasource-called true) nil)]
        (let [result (db/persist-manifest! nil {:snapshot "tank/x@backup-1"})]
          (is (= {:ok false :reason :no-connection-string} result))
          (is (false? @get-datasource-called)))))))

(deftest persist-manifest-inserts-when-conn-string-present
  (testing "calls jdbc/execute! with the manifest JSON when a connection string is given"
    (let [executed-args (atom nil)]
      (with-redefs [jdbc/get-datasource (fn [_] :fake-datasource)
                    jdbc/execute!       (fn [ds sql-params]
                                          (reset! executed-args [ds sql-params])
                                          [{}])]
        (let [manifest {:snapshot "tank/x@backup-1" :dataset "tank/x"}
              result   (db/persist-manifest! "postgres://localhost/db" manifest)]
          (is (= {:ok true} result))
          (is (some? @executed-args))
          (is (= :fake-datasource (first @executed-args)))
          (is (str/includes? (first (second @executed-args)) "INSERT"))
          (is (= "tank/x@backup-1" (second (second @executed-args)))))))))

(deftest persist-manifest-propagates-exceptions
  (testing "throws when the DB operation fails — caller handles fire-and-forget"
    (with-redefs [jdbc/get-datasource (fn [_] :fake-datasource)
                  jdbc/execute!       (fn [_ _] (throw (ex-info "connection refused" {})))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (db/persist-manifest! "postgres://localhost/db" {:snapshot "s"}))))))
