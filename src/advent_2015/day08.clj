(ns advent-2015.day08
  (:require [clojure.java.io :as io]
            [dev :as dev]))

(def input
  (line-seq (io/reader (io/resource "day08"))))

;;
;; Shorten each time by non-memory length of a character, so the sum of all these shortens
;; (where each shorten is given a length of 1) will be the memory length.
;; We are always looking at the next two, but may be advancing by 1, 2 or 4
;;
(defn unescape-trunc [input]
  ;(println (str "<to unescape: <" (seq input) "> <" (count input) ">>"))
  (drop (condp = (take 2 input)
          [\\ \"] 2
          [\\ \\] 2
          [\\ \x] 4
          1)
        input))

;;
;; To find the true (unescaped, in memory) length of x
;;
(defn memory-len [x]
  (- (-> (take-while not-empty
                     (iterate unescape-trunc x))
         dev/probe-off
         count)
     2))

(defn part-1 []
  (->> input
       ;(take 1)
       dev/probe-off
       (map #(- (count %) (dev/probe-off (memory-len %) "mem len")))
       (reduce + 0)))

;;
;; Always two more characters for the quotes
;;
(defn expand-len [w]
  (reduce + 2
          (map #(condp = %
                  \" 2
                  \\ 2
                  1)
               w)))

(defn part-2 []
  (reduce + 0 (map #(- (expand-len %) (count %)) input)))
