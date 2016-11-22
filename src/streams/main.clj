(ns streams.main
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as string])
  (:gen-class))

(defn index-a-file
  "Index words in FILE returning a map {word {file [locations ...]}}"
  [file]
  (->> file slurp
       (re-seq #"\w+")
       (map string/lower-case)
       (map (fn [index word] {word [index]}) (range))
       (reduce (partial merge-with into))
       (map (fn [[k v]] [k {file v}]))
       (into {})))

(defn index-the-files
  "Collate and sort the indexes of FILES."
  [files]
  (->> files
       (map index-a-file)
       (reduce (partial merge-with merge))
       (sort-by key)
       (into [])))

(defn count-words
  "For each word in INDEX, count its locations in each file."
  [index]
  (into [] (for [[word locations] index]
             [word (zipmap (keys locations)
                           (map count (vals locations)))])))

(defn -main [& args]
  (pprint
   (if (empty? args)
     (string/join \newline ["Usage: streams file [file ...]"
                            "Try: streams *.txt"])
     (let [indexed (index-the-files args)]
       {:counted (count-words indexed)
        :indexed indexed}))))

(def indexed
  (index-the-files
   ["child.txt" "contrary.txt" "mary.txt" "row.txt"]))

(def counted (count-words indexed))
