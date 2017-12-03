(ns advent-2017.day03-1
  (:require [utils :as u]))

;;
;; When number is even exiting square from left. When odd from bottom right.
;; Here we find the position where are existing the square.
;;
(defn last-whole-num-root [n]
  (let [corners (->> (range 1 n)
                     (map (juxt u/sqrt identity))
                     (filter #(u/whole-number? (first %)))
                     dev/probe-off)]
    (conj (last corners) (count corners))))

;;
;; For positioning up is +ive and left is -ive
;;
(def up [0 1])
(def down [0 -1])
(def left [-1 0])
(def right [1 0])

(defn ups [n] (repeat n up))
(defn downs [n] (repeat n down))
(defn lefts [n] (repeat n left))
(defn rights [n] (repeat n right))

;;
;; It is either the top left or bottom right, but
;; we are just going to record the change as a sequence
;; going from the origin to where we are.
;; At top left half the counted-sqrt is how many up, one less is how many across
;; At bottom right they are equal movements down and right
;;
(defn corner-movement [sqrt]
  (let [last-corner (* sqrt sqrt)
        ;; If not top-left? is bottom-right?
        top-left? (even? last-corner)
        half-sqrt (int (/ sqrt 2))]
    (if top-left?
      (concat (ups half-sqrt) (lefts (- half-sqrt 1)))
      (concat (downs half-sqrt) (rights half-sqrt)))))

;; For top-left? diff has two parts, down and right, that both go against what get from counted-sqrt.
;; We use the new length of a side, which is inc of counted-sqrt
(defn from-corner-movement [top-left? counted-sqrt diff]
  (let [side-length (inc counted-sqrt)
        [horiz-f vert-f opp-horiz-f] (if top-left?
                                       [lefts downs rights]
                                       [rights ups lefts])]
    (cond-> []
            (>= diff 1) (concat (horiz-f 1))
            true (concat (vert-f (dec (min diff side-length))))
            (> diff side-length) (concat (opp-horiz-f (- diff side-length)))
            )))

(defn moves->position [moves]
  (reduce
    (fn [acc ele]
      (mapv + acc ele))
    [0 0]
    moves))

(defn position-info [n]
  (let [inced-n (inc n)
        [last-sqrt idx counted-sqrt] (last-whole-num-root inced-n)
        last-escape (int (* last-sqrt last-sqrt))
        diff (- n idx)
        ]
    (assert (== counted-sqrt last-sqrt))
    {:n            n
     :counted-sqrt counted-sqrt
     :top-left?    (even? last-escape)
     :last-escape  last-escape
     :diff         diff
     }))

(defn info->answer [{:keys [counted-sqrt top-left? diff] :as info}]
  (let [corner->home (corner-movement counted-sqrt)
        pos->corner (from-corner-movement top-left? counted-sqrt diff)
        position (moves->position (concat corner->home pos->corner))]
    (assoc info :corner-move corner->home
                :back-move pos->corner
                :position (moves->position (concat corner->home pos->corner))
                :distance (apply + (map u/abs position))
                )))

;; At  9 have counted 3 and edge length is 3
;; At 16 have counted 4 and edge length is 4
;; At 25 have counted 5 and edge length is 5
;; Odds are at bottom right corner of a square
;; Evens are also but getting back is one more down than across
;; At 8 last escape was 4, which is even so is top left.
;;
(defn produce-table []
  (let [table-length 10]
    (->> (for [n (range 1 (inc table-length))]
           (position-info n))
         (map info->answer)
         ;(drop (- table-length 1))
         dev/pp
         )))

;; ans 552
(defn x-1 []
  (:distance (info->answer (position-info 325489))))
