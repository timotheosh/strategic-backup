(ns strategic-backup.infisical-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clj-infisical.core :as infisical-core]
            [strategic-backup.infisical :as infisical]))

;; ---------------------------------------------------------------------------
;; b2-rclone-env-vars
;; ---------------------------------------------------------------------------

(deftest b2-rclone-env-vars-shape
  (testing "returns exactly the three RCLONE_CONFIG_B2_* keys"
    (is (= {"RCLONE_CONFIG_B2_TYPE"    "b2"
            "RCLONE_CONFIG_B2_ACCOUNT" "my-key-id"
            "RCLONE_CONFIG_B2_KEY"     "my-app-key"}
           (infisical/b2-rclone-env-vars "my-key-id" "my-app-key")))))

;; **Validates: Requirement 3.3**
(defspec b2-rclone-env-vars-property 100
  (prop/for-all [key-id     (gen/not-empty gen/string-alphanumeric)
                 app-key    (gen/not-empty gen/string-alphanumeric)]
    (let [result (infisical/b2-rclone-env-vars key-id app-key)]
      (and (= #{"RCLONE_CONFIG_B2_TYPE" "RCLONE_CONFIG_B2_ACCOUNT" "RCLONE_CONFIG_B2_KEY"}
              (set (keys result)))
           (= "b2" (get result "RCLONE_CONFIG_B2_TYPE"))
           (= key-id (get result "RCLONE_CONFIG_B2_ACCOUNT"))
           (= app-key (get result "RCLONE_CONFIG_B2_KEY"))))))

;; ---------------------------------------------------------------------------
;; fetch-secret!
;; ---------------------------------------------------------------------------

(def ^:private sample-infisical-config
  {:project-id  "proj-123"
   :environment "prod"
   :secret-path "/backend"
   :site-url    "https://infisical.example.com"})

(deftest fetch-secret-calls-get-secret-with-merged-args
  (testing "passes project-id/environment/secret-path/site-url from infisical-config plus secret-name"
    (let [captured-args (atom nil)]
      (with-redefs [infisical-core/get-secret! (fn [args] (reset! captured-args args) "the-value")]
        (let [result (infisical/fetch-secret! sample-infisical-config "PGCONNSTRING")]
          (is (= "the-value" result))
          (is (= {:project-id  "proj-123"
                  :environment "prod"
                  :secret-path "/backend"
                  :site-url    "https://infisical.example.com"
                  :secret-name "PGCONNSTRING"}
                 @captured-args)))))))

(deftest fetch-secret-never-passes-client-credentials
  (testing "does not pass :client-id/:client-secret — relies on clj-infisical's own Universal Auth resolution (Req 4.1)"
    (let [captured-args (atom nil)]
      (with-redefs [infisical-core/get-secret! (fn [args] (reset! captured-args args) "v")]
        (infisical/fetch-secret! sample-infisical-config "PGCONNSTRING")
        (is (not (contains? @captured-args :client-id)))
        (is (not (contains? @captured-args :client-secret)))))))

(deftest fetch-secret-wraps-failure-into-staged-ex-info
  (testing "on clj-infisical failure, throws ex-info with :stage :secrets and the secret name, no value exposed"
    (with-redefs [infisical-core/get-secret!
                  (fn [_]
                    (throw (ex-info "Infisical secret fetch failed: secret-not-found"
                                    {:type   :clj-infisical/secret-not-found
                                     :status 404 :body "{}" :parsed {}})))]
      (try
        (infisical/fetch-secret! sample-infisical-config "B2_KEY_ID")
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :secrets (:stage (ex-data e))))
          (is (= "B2_KEY_ID" (:secret-name (ex-data e))))
          (is (= :clj-infisical/secret-not-found (:infisical-error (ex-data e))))
          (is (not (clojure.string/includes? (.getMessage e) "value")))
          (is (not (clojure.string/includes? (pr-str (ex-data e)) "the-value"))))))))

(deftest fetch-secret-wraps-auth-failure-the-same-way
  (testing "an Infisical Universal Auth failure is treated identically to any other fetch failure (Req 4.2)"
    (with-redefs [infisical-core/get-secret!
                  (fn [_]
                    (throw (ex-info "Infisical credential resolution failed: credentials-not-found"
                                    {:type :clj-infisical/credentials-not-found})))]
      (try
        (infisical/fetch-secret! sample-infisical-config "PGCONNSTRING")
        (is false "expected ex-info to be thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :secrets (:stage (ex-data e))))
          (is (= :clj-infisical/credentials-not-found (:infisical-error (ex-data e)))))))))
