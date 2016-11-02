#!/usr/bin/env boot

;; See: https://github.com/boot-clj/boot/wiki/Boot-Environment
;;
(set-env!
 :resource-paths #{"src"}
 :target-path "target"
 :dependencies '[[org.clojure/clojure "1.8.0"]])

(deftask link
  "Ensure ./streams links to this file, build.boot."
  []
  (with-pre-wrap fileset
    (when-not (.exists (clojure.java.io/as-file "./streams"))
      (dosh "ln" "-s" "./build.boot" "./streams"))
    fileset))

(deftask build
  "Build this."
  []
  (comp
   (link)
   (aot :namespace '#{streams.main})
   (pom :project 'streams :version "0.1.0")
   (uber)
   (jar :main 'streams.main
        :manifest {"Description" "Help Charlie with some Java streaming."
                   "Url" "https://github.com/charliesawyer/streams.git"})
   (target)))

(require '[boot.pod :as pod])

(defn -main
  "Run this."
  [& args]
  (pod/with-eval-in (pod/make-pod (get-env))
    (require 'streams.main)
    (apply streams.main/-main ~(vec args))))
