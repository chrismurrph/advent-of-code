(ns advent-2017.day03-1b
  (:require [utils :as u]))

;;
;; For positioning up is +ive and left is -ive
;;
(def north [0 1])
(def south [0 -1])
(def west [-1 0])
(def east [1 0])

(defn ups [n] (repeat n north))
(defn downs [n] (repeat n south))
(defn lefts [n] (repeat n west))
(defn rights [n] (repeat n east))

(defn moves->position [moves]
  (reduce
    (fn [acc ele]
      (mapv + acc ele))
    [0 0]
    moves))

(def directions
  {0 north
   1 east
   2 south
   3 west})

(defn move [from-pos new-dir]
  (let [dir-delta (directions new-dir)]
    (mapv + from-pos dir-delta)))

;; up is 0
;; east is 1
;; down is 2
;; west is 3
(defn turn-left [dir]
  (if (zero? dir)
    3
    (dec dir)))

(defn iteree [{:keys [n walk-straight pos dir side-size times-turned] :as st}]
  (let [left-turn? (and (= side-size (inc walk-straight)) (< times-turned 2))
        new-dir (if left-turn? (turn-left dir) dir)
        new-pos (move pos new-dir)
        new-walk-straight (if left-turn? 0 (inc walk-straight))
        new-side-size (cond-> side-size
                              (>= times-turned 2) inc)
        ]
    {:n             (inc n)
     :walk-straight new-walk-straight
     :pos           new-pos
     :dir           new-dir
     :side-size     new-side-size
     :times-turned  (cond-> times-turned
                            left-turn? inc)}))

(def start-state {:n 1
                  :walk-straight 1
                  :pos [0 0]
                  :dir 1
                  :side-size 2
                  :times-turned 0})

(defn finished? [{:keys [n]}]
  (= 3 n))

;; ans 552
(defn x-1 []
  (->> (iterate iteree start-state)
       (take 3)
       ;(drop-while (complement finished?))
       ;first
       ))
