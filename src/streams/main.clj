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

(defn clean-words
  "A lazy sequence of lower-cased words in FILE."
  [file]
  (->> file io/reader word-seq
       (map string/lower-case)))

(defn count-words
  "Count words in FILE returning a map {word {file count} ...}"
  [file]
  (->> file clean-words frequencies
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
  (if (seq args)
    (pprint {:counted (collate count-words args)
             :indexed (collate index-words args)})
    (println
     (string/join \newline
                  ["Usage: streams file [file ...]"
                   "Try: streams *.txt"]))))

(comment
  "Result of: ./streams child.txt contrary.txt mary.txt row.txt"
  {:counted [["a" {"child.txt" 1,"contrary.txt" 1,"mary.txt" 1,"row.txt" 1}]
             ["all" {"contrary.txt" 1}]
             ["and" {"child.txt" 4, "contrary.txt" 2, "mary.txt" 1}]
             ["anywhere" {"mary.txt" 1}]
             ["as" {"mary.txt" 1}]
             ["bells" {"contrary.txt" 1}]
             ["boat" {"row.txt" 1}]
             ["born" {"child.txt" 1}]
             ["but" {"child.txt" 1, "row.txt" 1}]
             ["child" {"child.txt" 7}]
             ["cockleshells" {"contrary.txt" 1}]
             ["contrary" {"contrary.txt" 1}]
             ["day" {"child.txt" 1}]
             ["does" {"contrary.txt" 1}]
             ["down" {"row.txt" 1}]
             ["dream" {"row.txt" 1}]
             ["face" {"child.txt" 1}]
             ["fair" {"child.txt" 2}]
             ["far" {"child.txt" 1}]
             ["fleece" {"mary.txt" 1}]
             ["for" {"child.txt" 1}]
             ["friday's" {"child.txt" 1}]
             ["full" {"child.txt" 2}]
             ["garden" {"contrary.txt" 1}]
             ["gay" {"child.txt" 1}]
             ["gently" {"row.txt" 1}]
             ["giving" {"child.txt" 1}]
             ["go" {"child.txt" 1, "mary.txt" 1}]
             ["good" {"child.txt" 1}]
             ["grace" {"child.txt" 1}]
             ["grow" {"contrary.txt" 1}]
             ["had" {"mary.txt" 1}]
             ["has" {"child.txt" 1}]
             ["how" {"contrary.txt" 1}]
             ["in" {"contrary.txt" 1}]
             ["is" {"child.txt" 5, "row.txt" 1}]
             ["its" {"mary.txt" 1}]
             ["lamb" {"mary.txt" 2}]
             ["life" {"row.txt" 1}]
             ["little" {"mary.txt" 1}]
             ["living" {"child.txt" 1}]
             ["loving" {"child.txt" 1}]
             ["maids" {"contrary.txt" 1}]
             ["mary" {"contrary.txt" 2, "mary.txt" 2}]
             ["merrily" {"row.txt" 4}]
             ["monday's" {"child.txt" 1}]
             ["must" {"child.txt" 1}]
             ["of" {"child.txt" 3}]
             ["on" {"child.txt" 1}]
             ["pretty" {"contrary.txt" 1}]
             ["quite" {"contrary.txt" 1}]
             ["row" {"contrary.txt" 1, "row.txt" 3}]
             ["sabbath" {"child.txt" 1}]
             ["saturday's" {"child.txt" 1}]
             ["silver" {"contrary.txt" 1}]
             ["snow" {"mary.txt" 1}]
             ["stream" {"row.txt" 1}]
             ["sure" {"mary.txt" 1}]
             ["that" {"mary.txt" 1}]
             ["that's" {"child.txt" 1}]
             ["the" {"child.txt" 2, "mary.txt" 1, "row.txt" 1}]
             ["thursday's" {"child.txt" 1}]
             ["to" {"child.txt" 1, "mary.txt" 1}]
             ["tuesday's" {"child.txt" 1}]
             ["was" {"mary.txt" 2}]
             ["wednesday's" {"child.txt" 1}]
             ["went" {"mary.txt" 1}]
             ["white" {"mary.txt" 1}]
             ["wise" {"child.txt" 1}]
             ["with" {"contrary.txt" 1}]
             ["woe" {"child.txt" 1}]
             ["work" {"child.txt" 1}]
             ["your" {"contrary.txt" 1, "row.txt" 1}]],
   :indexed [["a" {"child.txt" [35],
                   "contrary.txt" [19],
                   "mary.txt" [2],
                   "row.txt" [16]}]
             ["all" {"contrary.txt" [17]}]
             ["and" {"child.txt" [28 48 50 52],
                     "contrary.txt" [12 14],
                     "mary.txt" [11]}]
             ["anywhere" {"mary.txt" [12]}]
             ["as" {"mary.txt" [9]}]
             ["bells" {"contrary.txt" [11]}]
             ["boat" {"row.txt" [4]}]
             ["born" {"child.txt" [41]}]
             ["but" {"child.txt" [37], "row.txt" [15]}]
             ["child" {"child.txt" [1 7 13 19 25 31 39]}]
             ["cockleshells" {"contrary.txt" [13]}]
             ["contrary" {"contrary.txt" [3]}]
             ["day" {"child.txt" [45]}]
             ["does" {"contrary.txt" [5]}]
             ["down" {"row.txt" [6]}]
             ["dream" {"row.txt" [17]}]
             ["face" {"child.txt" [5]}]
             ["fair" {"child.txt" [3 47]}]
             ["far" {"child.txt" [21]}]
             ["fleece" {"mary.txt" [6]}]
             ["for" {"child.txt" [34]}]
             ["friday's" {"child.txt" [24]}]
             ["full" {"child.txt" [9 15]}]
             ["garden" {"contrary.txt" [7]}]
             ["gay" {"child.txt" [53]}]
             ["gently" {"row.txt" [5]}]
             ["giving" {"child.txt" [29]}]
             ["go" {"child.txt" [23], "mary.txt" [21]}]
             ["good" {"child.txt" [51]}]
             ["grace" {"child.txt" [11]}]
             ["grow" {"contrary.txt" [8]}]
             ["had" {"mary.txt" [1]}]
             ["has" {"child.txt" [20]}]
             ["how" {"contrary.txt" [4]}]
             ["in" {"contrary.txt" [18]}]
             ["is" {"child.txt" [2 8 14 26 46], "row.txt" [14]}]
             ["its" {"mary.txt" [5]}]
             ["lamb" {"mary.txt" [4 17]}]
             ["life" {"row.txt" [13]}]
             ["little" {"mary.txt" [3]}]
             ["living" {"child.txt" [36]}]
             ["loving" {"child.txt" [27]}]
             ["maids" {"contrary.txt" [16]}]
             ["mary" {"contrary.txt" [0 1], "mary.txt" [0 14]}]
             ["merrily" {"row.txt" [9 10 11 12]}]
             ["monday's" {"child.txt" [0]}]
             ["must" {"child.txt" [32]}]
             ["of" {"child.txt" [4 10 16]}]
             ["on" {"child.txt" [42]}]
             ["pretty" {"contrary.txt" [15]}]
             ["quite" {"contrary.txt" [2]}]
             ["row" {"contrary.txt" [20], "row.txt" [0 1 2]}]
             ["sabbath" {"child.txt" [44]}]
             ["saturday's" {"child.txt" [30]}]
             ["silver" {"contrary.txt" [10]}]
             ["snow" {"mary.txt" [10]}]
             ["stream" {"row.txt" [8]}]
             ["sure" {"mary.txt" [19]}]
             ["that" {"mary.txt" [13]}]
             ["that's" {"child.txt" [40]}]
             ["the" {"child.txt" [38 43], "mary.txt" [16], "row.txt" [7]}]
             ["thursday's" {"child.txt" [18]}]
             ["to" {"child.txt" [22], "mary.txt" [20]}]
             ["tuesday's" {"child.txt" [6]}]
             ["was" {"mary.txt" [7 18]}]
             ["wednesday's" {"child.txt" [12]}]
             ["went" {"mary.txt" [15]}]
             ["white" {"mary.txt" [8]}]
             ["wise" {"child.txt" [49]}]
             ["with" {"contrary.txt" [9]}]
             ["woe" {"child.txt" [17]}]
             ["work" {"child.txt" [33]}]
             ["your" {"contrary.txt" [6], "row.txt" [3]}]]}
  )
