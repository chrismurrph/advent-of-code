(ns hackerrank.two-strings
  (:require [clojure.string :as str]))

(def input ["2"
            "hello"
            "world"
            "hi"
            "world"])

(defn common-1 [[left right]]
  (let [together-1 (for [i (take 10 left)
                         j (take 10 right)]
                     [i j])
        together-2 (for [i (take 50 left)
                         j (take 50 right)]
                     [i j])
        together-3 (for [i left
                         j right]
                     [i j])
        res (first (filter (fn [[l r]] (= l r)) together-1))]
    (or res (first (filter (fn [[l r]] (= l r)) together-2)) (first (filter (fn [[l r]] (= l r)) together-3)))
    ))

(defn common-2 [[left right]]
  (let [together (for [i left]
                   i)]
    (first (filter (fn [l] (str/index-of right l)) together))
    ))

(defn x []
  (let [str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (clojure.string/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        pairs (partition 2 (next input))
        res (map (memoize common-1) pairs)
        res-2 (for [i res]
                (if i "YES" "NO"))
        ]
    (doseq [i res-2]
      (println i))
    ))
