(defproject strategic-backup "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://example.com/FIXME"
  :license {:name "MIT License"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.12.4"]
                 [com.taoensso/timbre "6.6.1"]
                 [org.postgresql/postgresql "42.7.4"]
                 [com.github.seancorfield/next.jdbc "1.3.939"]
                 [org.clojure/data.json "2.5.1"]
                 [org.clojure/test.check "1.1.1"]
                 [clojure.java-time/clojure.java-time "1.4.2"]
                 [io.github.timotheosh/clj-infisical "0.1.0"]]
  :main ^:skip-aot strategic-backup.core
  :target-path "target/%s"
  ;; Fixed filename (not <name>-<version>-standalone.jar) so cron entries
  ;; and deploy scripts never need to change on a version bump, and don't
  ;; need to glob/regex for the artifact.
  :uberjar-name "strategic-backup.jar"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[org.clojure/test.check "1.1.1"]]}})
