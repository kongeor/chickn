(defproject chickn "0.1.0-SNAPSHOT"
  :description "clojure(script) genetic algorithms toolkit"
  :url "https://github.com/kongeor/chickn"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.10"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :aliases {"testjs"  ["doo" "phantom" "test" "once"]
            "testall" ["do" "clean" ["test"] ["doo" "phantom" "test" "once"]]}
  :cljsbuild {:builds [{:id "test"
                        :source-paths ["src" "test"]
                        :compiler {:output-to "resources/public/js/testable.js"
                                   :main chickn.runner
                                   :optimizations :none}}]})
