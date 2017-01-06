(ns hackerrank.picking-numbers
  (:require [utils :as u]
            [clojure.string :as s]))

(def input ["6"
            "4 6 5 3 3 1"])

;; want 5
(def input-2 ["6"
              "1 1 2 2 3 3 3 1 2"])

(defn abs [val]
  (if (neg? val)
    (* -1 val)
    val))

(defn x []
  (let [str->ints (fn [string]
                    (map #(Integer/parseInt %)
                         (s/split string #" ")))
        ;input (line-seq (java.io.BufferedReader. *in*))
        numbers (str->ints (first (next input-2)))
        freqs (reverse (sort-by second (frequencies numbers)))
        highest (ffirst freqs)
        within? (fn [[num freq]]
                  (let [diff (abs (- num highest))]
                    (<= diff 1)))
        only-nears (take 2 (filter within? freqs))
        _ (println only-nears)
        res (apply + (map second only-nears))
        ]
    res
    ))
