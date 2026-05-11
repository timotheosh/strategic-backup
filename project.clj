(defproject strategic-backup "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "https://example.com/FIXME"
  :license {:name "MIT License"
            :url "https://mit-license.org/"}
  :dependencies [[org.clojure/clojure "1.12.4"]]
  :main ^:skip-aot strategic-backup.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
