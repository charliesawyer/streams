(ns streams.main
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string])
  (:gen-class))

(defn byte-seq
  "A lazy sequence of bytes from READER."
  [reader]
  (let [b (. reader read)]
    (if (>= b 0)
      (cons b (lazy-seq (byte-seq reader))))))

(defn word-seq
  "A lazy sequence of words from READER."
  [reader]
  (letfn [(alpha? [c] (Character/isAlphabetic c))
          (word? [[w & ord]] (alpha? w))
          (gather [ints] (apply str (map char ints)))]
    (->> reader byte-seq
         (partition-by alpha?)
         (filter word?)
         (map gather))))

(defn index-a-file
  "Index words in FILE returning a map {word {file [locations ...]}}"
  [file]
  (with-open [reader (io/reader file)]
    (->> reader word-seq
         (map string/lower-case)
         (map (fn [index word] {word [index]}) (range))
         (reduce (partial merge-with into))
         (map (fn [[k v]] [k {file v}]))
         (into {}))))

(defn index-the-files
  "Collate and sort the indexes of FILES."
  [files]
  (->> files
       (map index-a-file)
       (reduce (partial merge-with merge))
       (sort-by key)
       (into [])))

(defn count-words
  "For each word in INDEXED, count its locations in each file."
  [indexed]
  (into [] (for [[word locations] indexed]
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

(comment
  (-main "child.txt" "contrary.txt" "mary.txt" "row.txt")
  )
