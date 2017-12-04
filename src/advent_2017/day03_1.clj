(ns advent-2017.day03-1
  (:require [utils :as u]))

;;
;; For positioning up is +ive and left is -ive
;;
(def north [0 1])
(def south [0 -1])
(def west [-1 0])
(def east [1 0])

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

(defn iteree [{:keys [n pos dir already-visited]}]
  (let [new-already-visited (conj already-visited pos)
        at-left (move pos (turn-left dir))
        left-occupied? (new-already-visited at-left)
        [new-pos new-dir] (if left-occupied?
                            [(move pos dir) dir]
                            [at-left (turn-left dir)])]
    {:n   (inc n)
     :pos new-pos
     :dir new-dir
     :already-visited new-already-visited
     }))

;; Facing south so first turn will be to the east
(def start-state {:n               1
                  :pos             [0 0]
                  :dir             2
                  ;; Only need to keep the most recent of these (however we keep all)
                  :already-visited #{}
                  })

(defn finished? [{:keys [n]}]
  (= 325489 n))

;; ans 552
(defn x-1 []
  (->> (iterate iteree start-state)
       (drop-while (complement finished?))
       first
       :pos
       (map u/abs)
       (apply +)
       ))
