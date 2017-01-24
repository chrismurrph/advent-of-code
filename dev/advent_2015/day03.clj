(ns advent-2015.day03
  (:require [clojure.java.io :as io]
            [utils :as u]))

(def test-input "^v^v^v^v^v")

;;
;; Lets have Sanata starting at [0 0] and make this cartesian - like school graphs
;;
(def directions {\v [0 -1]
                 \^ [0 1]
                 \> [1 0]
                 \< [-1 0]
                 \. [0 0]})

(defn move [[delta-x delta-y :as delta] [curr-x curr-y :as curr]]
  ;(println "->" delta curr "<-")
  [(+ delta-x curr-x) (+ delta-y curr-y)])

;; 1762 is too low, however 2572 is correct - the question was all the houses that get presents, not just the
;; lucky ones
(defn part-1 []
  (let [
        input test-input
        ;input (slurp (io/resource "day03"))
        ]
    (->> input
         (reductions (fn [acc ele] (move (directions ele) acc)) [0 0])
         ;(take 10)
         sort
         (partition-by identity)
         (filter #(> (count %) 0))
         count)))

(defn part-2 []
  (let [
        ;input test-input
        input (slurp (io/resource "day03"))
        santa-visits (->> input
                          (partition 2)
                          (map first)
                          (reductions (fn [acc ele] (move (directions ele) acc)) [0 0]))
        robo-visits (->> input
                         (partition 2)
                         (map second)
                         (reductions (fn [acc ele] (move (directions ele) acc)) [0 0]))
        altogether (->> (concat santa-visits robo-visits)
                        sort
                        (partition-by identity))
        ]
    (count altogether)))
