(ns advent.eight
  (:require [instaparse.core :as insta]
            [utils :as u])
  (:import (java.io StringReader BufferedReader)))

;;
;; rect 4x1
;; rotate row y=0 by 4
;; rotate column x=0 by 1
;;
(def test-input ["rect 4x1" "rotate row y=0 by 4" "rotate column x=0 by 1"])

(def grammar
  (insta/parser
    "<S> = rect | rotate
     int = #'[0-9]+'
     direction = 'row' | 'column'
     rect = <'rect '> int <'x'> int
     var = 'x' | 'y'
     rotate = <'rotate '> direction <' ' var '='> int <' by '> int"))

(defn to-hiccup [s]
  (->> (first (grammar s))
       (insta/transform
         {:int clojure.edn/read-string :direction (fn [in] (case in "row" :row "column" :col))}))
  )

;;
;; Add up the areas of all the rectangles
;;
(defn first-part-correct []
  (let [input test-input
        input (slurp "./advent/eight.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        ]
    (->> raw-series
         (map to-hiccup)
         (filter #(= :rect (-> % first)))
         (map #(* (second %) (nth % 2)))
         (reduce +)
         )
    ))

(def nothing " ")
(def something "x")

(defn make-row [total-cols]
  (fn [_]
    (vec (repeat total-cols nothing))))

(defn turn
  "Turn a pixel on or off"
  [b? state x y]
  (-> state
      (update y (fn [old-row]
                  (-> old-row
                      (assoc x b?))))))
(defn turn-on [state x y]
  (turn something state x y))
(defn turn-off [state x y]
  (turn nothing state x y))

(defn read-pixel [state]
  (fn [[x y]]
    (nth (nth state y) x)))

(defn rotator [total]
  (fn [distance]
    (fn [existing-x]
      (rem (+ existing-x distance) total))))

(defn rotate [axis total state row-or-col-num distance]
  (let [existing-xs-or-ys (range total)
        whole-row-or-column (case axis
                              :axis/row (for [x existing-xs-or-ys]
                                          [x row-or-col-num])
                              :axis/column (for [y existing-xs-or-ys]
                                             [row-or-col-num y]))
        read-f (read-pixel state)
        all-values (mapv read-f whole-row-or-column)
        ;_ (println (str "All values of row " row-or-col-num ": " all-values))
        new-xs (map ((rotator total) distance) existing-xs-or-ys)
        whole-new-row-or-column (case axis
                                  :axis/row (for [x new-xs]
                                              [x row-or-col-num])
                                  :axis/column (for [y new-xs]
                                                 [row-or-col-num y]))
        ;_ (println "existing row: " whole-row-or-column)
        ;_ (println "new row: " whole-new-row-or-column)
        changlings (map vector whole-new-row-or-column all-values)
        ;_ (println "changlings" changlings)
        ]
    (reduce
      (fn [acc [[x y] b?]]
        (turn b? acc x y))
      state
      changlings)))

(defn rotate-row [total-cols]
  (fn [state row-num distance]
    (rotate :axis/row total-cols state row-num distance)))

(defn rotate-column [total-rows]
  (fn [state col-num distance]
    (rotate :axis/column total-rows state col-num distance)))

(defn rect-on
  "Put a rect in top left corner"
  [state col-count row-count]
  (let [product (for [y (range row-count)
                      x (range col-count)]
                  [x y])]
    (reduce
      (fn [acc [x y]]
        (turn-on acc x y))
      state
      product)))

#_(defn x-1 []
  (let [state (mapv make-row (range total-rows))
        new-state (rect-on state 3 2)]
    (rotate-column new-state 1 1)
    ))

(defn second-part [total-rows total-cols]
  (let [raw-series test-input
        input (slurp "./advent/eight.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        row-maker (make-row total-cols)
        state (mapv row-maker (range total-rows))
        instructions (->> raw-series
                          (map to-hiccup))
        _ (println instructions)
        row-rotator (rotate-row total-cols)
        column-rotator (rotate-column total-rows)
        ]
    (reduce
      (fn [acc [cmd & tail]]
        (case cmd
          :rect (rect-on acc (first tail) (second tail))
          :rotate (let [[selector row-or-col quantity] tail]
                    (case selector
                      :row (row-rotator acc row-or-col quantity)
                      :col (column-rotator acc row-or-col quantity))))
        )
      state
      instructions)))

;; 50 7
(def total-cols 50)

;; 6 3
(def total-rows 6)

(defn x []
  (let [res (second-part total-rows total-cols)]
    res))
