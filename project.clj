(defproject chickn "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/clojurescript "1.9.293"]]
  :plugins [[lein-cljsbuild "1.1.4"]
            [lein-doo "0.1.7"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :aliases {"testjs"  ["doo" "phantom" "test" "once"]
            "testall" ["do" "clean" ["test"] ["doo" "phantom" "test" "once"]]}
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:output-to "resources/public/js/testable.js"
                                   :main chickn.runner
                                   :optimizations :none}}]})
