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

(defn count-words
  "Count words in FILE returning a map {word {file count} ...}"
  [file]
  (->> file clean-words
       frequencies
       (map (fn [[k v]] [k {file v}]))
       (into {})))

(defn index-words
  "Index words in FILE returning a map {word {file [indexes ...]}}"
  [file]
  (->> file clean-words
       (map (fn [index word] {word [index]}) (range))
       (reduce (partial merge-with into))
       (map (fn [[k v]] [k {file v}]))
       (into {})))

(defn collate
  "Collate and sort the result of applying MAP-WORDS to FILES."
  [map-words files]
  (->> files
       (map map-words)
       (reduce (partial merge-with merge))
       (sort-by key)))

(defn -main [& args]
  (pprint
   (if (seq args)
     {:counted (collate count-words args)
      :indexed (collate index-words args)}
     "Usage: streams file [file ...]")))
