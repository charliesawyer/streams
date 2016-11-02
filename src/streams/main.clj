(ns streams.main
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string])
  (:gen-class))

(defn clean-words
  "Return a sequence of lower-cased words in FILE."
  [file]
  (->> file slurp
       (re-seq  #"[-A-Za-z']+")
       (map string/lower-case)))

(defn frob-with
  "FROB words in FILE and return a map {word {file [indexes ...]}}.
  Where (frob words) returns the sequence of maps {word frobbed}."
  [frob]
  (fn [file] (->> file clean-words frob
                  (map (fn [[k v]] [k {file v}]))
                  (into {}))))

(defn index-words
  "Index WORDS returning a map {word [index ...]}"
  [words]
  (->> words
       (map (fn [index word] {word [index]}) (range))
       (reduce (partial merge-with into))))

(defn collate
  "Collate and sort the result of applying MAP-WORDS to FILES."
  [map-words files]
  (->> files
       (map map-words)
       (reduce (partial merge-with merge))
       (sort-by key)))

(defn -main [& args]
  (pprint (if (seq args)
            {:counted (collate (frob-with frequencies) args)
             :indexed (collate (frob-with index-words) args)}
            "Usage: streams file [file ...]")))

;; (-main "child.txt" "contrary.txt" "mary.txt" "row.txt")
