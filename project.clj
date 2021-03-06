(defproject advent-of-code "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/core.async "0.2.395"]
                 [com.taoensso/timbre "4.3.1"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [instaparse "1.4.2"]
                 [proto-repl "0.3.1"]
                 [medley "0.8.4"]
                 [digest "1.4.4"]
                 [org.clojure/math.combinatorics "0.1.1"]]

  :main ^:skip-aot parser.core
  :target-path "target/%s"

  :clean-targets ^{:protect false} ["target"]
  :source-paths ["src"]

  :profiles {:dev {
                   :repl-options {
                                  :init-ns          user
                                  :port             7001}

                   :env          {:dev true}
                   :dependencies [[org.clojure/test.check "0.9.0"]
                                  [binaryage/devtools "0.5.2" :exclusions [environ]]
                                  [org.clojure/java.classpath "0.2.3"]
                                  [org.clojure/tools.namespace "0.2.11"]]}})
