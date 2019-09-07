(defproject forman "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/tools.reader "1.3.2"]]
  :plugins [[io.taylorwood/lein-native-image "0.3.0"]]

  :main  ^:skip-aot forman.core
  :uberjar-name "forman.jar"
  :repl-options {:init-ns forman.core}
  :native-image {:opts ["--verbose"
                        "--no-fallback"
                        "--no-server"
                        "--initialize-at-build-time"
                        "--enable-url-protocols=http,https"
                        "-H:ReflectionConfigurationFiles=reflection.json"
                        "-H:+ReportExceptionStackTraces"
                        "--report-unsupported-elements-at-runtime"]
                 :name "forman"}

  :profiles {:uberjar {:aot :all
                       :native-image {:opts ["-Dclojure.compiler.direct-linking=true"]}}})
