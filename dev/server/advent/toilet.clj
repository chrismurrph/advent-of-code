(ns advent.toilet
  (:require [clojure.string :as str])
  (:import (java.io StringReader BufferedReader)))

(defn upf [out-bounds max-row]
  (fn [[x y]]
    (let [new-y (if (= y max-row) y (inc y))]
      (if (out-bounds [x new-y])
        [x y]
        [x new-y]))))

(defn downf [out-bounds]
  (fn [[x y]]
    (let [new-y (if (= y 1) y (dec y))]
      (if (out-bounds [x new-y])
        [x y]
        [x new-y]))))

(defn leftf [out-bounds]
  (fn [[x y]]
    (let [new-x (if (= x 1) x (dec x))]
      (if (out-bounds [new-x y])
        [x y]
        [new-x y]))))

(defn rightf [out-bounds max-col]
  (fn [[x y]]
    (let [new-x (if (= x max-col) x (inc x))]
      (if (out-bounds [new-x y])
        [x y]
        [new-x y]))))

(defn keypad [outbounds num-rows num-cols]
  (println outbounds)
  (let [up-f (upf outbounds num-rows)
        down-f (downf outbounds)
        left-f (leftf outbounds)
        right-f (rightf outbounds num-cols)]
    (fn [start-at instructions]
      (reduce
        (fn [acc ele]
          (let [f (case ele
                    :up up-f
                    :left left-f
                    :right right-f
                    :down down-f)]
            (f acc)))
        start-at
        instructions))))

(def up (first "U"))
(def left (first "L"))
(def right (first "R"))
(def down (first "D"))

(defn trans [line]
  (mapv #(cond
           (= % up) :up
           (= % left) :left
           (= % right) :right
           (= % down) :down
           ) line))

(def matrix-1 [[1 2 3] [4 5 6] [7 8 9]])
(def outbounds-1 #{})
(def matric-width-1 3)
(def start-loc-1 [2 2])

(def matrix-2 [['1]
               ['2 '3 '4]
               ['5 '6 '7 '8 '9]
               ['A 'B 'C]
               ['D]])
(def outbounds-2 #{[1 1] [1 2] [2 1] [4 1] [5 1] [5 2] [1 4] [1 5] [2 5] [4 5] [5 4] [5 5]})
(def matric-width-2 5)
(def start-loc-2 [1 3])

(def use-2 true)

(def my-matrix (if use-2 matrix-2 matrix-1))
(def outbounds (if use-2 outbounds-2 outbounds-1))
(def matrix-width (if use-2 matric-width-2 matric-width-1))
(def start-loc (if use-2 start-loc-2 start-loc-1))

(defn find-locations [start-at pad series]
  (next (reduce
          (fn [acc ele]
            (conj acc (pad (last acc) ele)))
          [start-at]
          series)))

(defn centre-in [int v]
  (let [fill-count (- int (count v))
        half-fill-count (/ fill-count 2)
        fill (repeat half-fill-count 'x)]
    (concat fill v fill)))

(defn pad-to [matrx num]
  (let [f (partial centre-in num)]
    (map f matrx)))

(defn trans-to-num [matrx]
  (let [
        ;padded-matrix (pad-to matrix matrix-width)
        num-rows (-> matrx first count)
        ;num-cols (-> matrix count)
        ]
    (fn [[x y]]
      (let [clump (- num-rows y)
            idx (dec x)
            ;_ (println (str clump "," idx))
            ]
        (nth (nth matrx clump) idx))))
  )

(defn x []
  (let [input (slurp "./advent/toilet.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        ;_ (println (count raw-series))
        series [[:up :left :left]
                [:right :right :down :down :down]
                [:left :up :right :down :left]
                [:up :up :up :up :down]
                ]
        series (mapv trans raw-series)
        ;_ (println series)
        padded-matrix (pad-to my-matrix matrix-width)
        num-rows (-> padded-matrix first count)
        num-cols (-> padded-matrix count)
        pad (keypad outbounds num-rows num-cols)
        locs (find-locations start-loc pad series)
        res (map (trans-to-num padded-matrix) locs)
        ]
    res
    ))
