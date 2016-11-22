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
          (word? [w] (alpha? (first w)))
          (gather [ints] (apply str (map char ints)))]
    (->> reader byte-seq
         (partition-by alpha?)
         (filter word?)
         (map gather))))

(defn index-a-file
  "Index words in FILE returning a map {word {file [indexes ...]}}"
  [file]
  (->> file io/reader word-seq
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
       (sort-by key)))

(defn count-words
  [index]
  (letfn [(count-locs [locs]
            (zipmap (keys locs)
                    (map count (vals locs))))]
    (for [[word locs] index]
      [word (count-locs locs)])))

(defn -main [& args]
  (pprint
   (if (seq args)
     (let [index (index-the-files args)]
       {:counted (count-words index)
        :indexed index})
     (string/join \newline
                  ["Usage: streams file [file ...]"
                   "Try: streams *.txt"]))))

(comment
  
  )
