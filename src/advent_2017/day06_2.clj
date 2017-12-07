(ns advent-2017.day06-2
  (:require [stopwatch :as sw]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [utils :as u]))

(defn to-ints [xs]
  (mapv #(Integer/parseInt %) xs))

(defn get-input []
  (->> (io/resource "2017/day06")
       slurp
       s/split-lines
       (mapv (comp to-ints #(re-seq #"\d+" %)))
       first))

(defn finished? [[memory-bank-blocks already-done]]
  (first (filter #(= % memory-bank-blocks) already-done)))

(defn biggest-block [blocks]
  (let [biggest (apply max blocks)
        first-idx (utils/index-of blocks biggest)]
    [first-idx biggest]))

(defn redistribute-hof [num-memory-banks]
  (fn [[before-memory-banks already-done]]
    (let [[biggest-idx num-at-biggest] (biggest-block before-memory-banks)
          after-memory-banks (reduce (fn [banks idx]
                                       (update banks idx inc))
                                     (assoc before-memory-banks biggest-idx 0)
                                     (->> #_(mapcat identity (repeat (range num-memory-banks)))
                                          (cycle (range num-memory-banks))
                                          (drop (inc biggest-idx))
                                          (take num-at-biggest)))
          history (conj already-done before-memory-banks)]
      [after-memory-banks history])))

;; Now that we know the x-2 requirement, have re-coded x-1
;; to no longer use a set for the history
;; ans: 3156
(defn x-1 []
  (let [tell-elapsed (sw/time-probe-hof "memory blocks")
        in #_[0 2 7 0] (get-input)
        start-state [in []]
        transition (redistribute-hof (count in))]
    (->> (iterate transition start-state)
         (drop-while (complement finished?))
         first
         ((comp count second))
         tell-elapsed
         )))

;; ans: 1610
(defn x-2 []
  (let [tell-elapsed (sw/time-probe-hof "memory blocks")
        in #_[0 2 7 0] (get-input)
        start-state [in []]
        transition (redistribute-hof (count in))]
    (->> (iterate transition start-state)
         (drop-while (complement finished?))
         first
         ((fn [[got-again history]]
            (let [idx (u/index-of history got-again)]
              (- (count history) idx))))
         tell-elapsed
         )))

(deftest first-wins
  (is (= [0 2 3 4]
         (first ((redistribute-hof 4) [[3 1 2 3] []])))))

(deftest first-chosen
  (let [[biggest-idx _] (biggest-block [3 1 2 3])]
    (is (= 0 biggest-idx))))
