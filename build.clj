(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.github.kongeor/chickn)
(defn- the-version [patch] (format "0.1.%s" patch))
(def version (the-version (b/git-count-revs nil)))
(def snapshot (the-version "999-SNAPSHOT"))
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_]
  (b/delete {:path "target"})
  (b/delete {:path "cljs-test-runner-out"}))

(defn- pom-template [version]
       [[:description "Evolutionary algorithms library for Clojure(script)"]
        [:url "https://github.com/kongeor/chickn"]
        [:licenses
         [:license
          [:name "Eclipse Public License"]
          [:url "http://www.eclipse.org/legal/epl-v10.html"]]]
        [:developers
         [:developer
          [:name "Kostas Georgiadis"]]]
        [:scm
         [:url "https://github.com/kongeor/chickn"]
         [:connection "scm:git:https://github.com/kongeor/chickn.git"]
         [:developerConnection "scm:git:ssh:git@github.com:kongeor/chickn.git"]
         [:tag (str "v" version)]]])

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis @basis
                :src-dirs ["src"]
                :pom-data (pom-template version)})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))
