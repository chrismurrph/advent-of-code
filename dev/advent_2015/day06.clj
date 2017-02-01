(ns advent-2015.day06
  (:require [clojure.java.io :as io]
            [utils :as u]))

(def input (line-seq (io/reader (io/resource "day06"))))

(defn mk-state [value width height]
  (vec (repeat height (vec (repeat width value)))))

(defn get-value [st [x y]]
  (nth (nth st y) x))

(defn set-value [st [x y] b?]
  (assoc-in st [y x] b?))

(defn turn-off [st coord]
  (set-value st coord false))

(defn turn-on [st coord]
  (set-value st coord true))

(defn toggle [st [x y]]
  (let [change-to? (not (get-value st [x y]))]
    (set-value st [x y] change-to?)))

(defn change-value [st [x y] f]
  (update-in st [y x] (fn [old-val]
                        (let [new-val (f old-val)]
                          (if (neg? new-val)
                            0
                            new-val)))))

(defn lower [st [x y]]
  (change-value st [x y] dec))

(defn higher [st [x y]]
  (change-value st [x y] inc))

(defn much-higher [st [x y]]
  (change-value st [x y] (comp inc inc)))

(defn create-cords [[top-left-x top-left-y] [bottom-right-x bottom-right-y]]
  (assert (and (< top-left-x bottom-right-x) (< top-left-y bottom-right-y)))
  (for [x (range top-left-x (inc bottom-right-x))
        y (range top-left-y (inc bottom-right-y))]
    [x y]))

;; 1,629 through 802,633
(defn parse-input [x]
  (->> (re-find #"(toggle|turn on|turn off) (\d+),(\d+) through (\d+),(\d+)" x)
       rest
       ((juxt first (comp u/string->int second) (comp u/string->int #(nth % 2)) (comp u/string->int #(nth % 3)) (comp u/string->int #(nth % 4))))
       ))

(defn do-cmd [turn-off turn-on toggle]
  (fn [st [cmd top-left-x top-left-y bottom-right-x bottom-right-y]]
    (assert (every? number? [top-left-x top-left-y bottom-right-x bottom-right-y]))
    (let [coords (create-cords [top-left-x top-left-y] [bottom-right-x bottom-right-y])]
      ;(println "num:" (count coords))
      (case cmd
        "turn off"
        (reduce turn-off st coords)
        "turn on"
        (reduce turn-on st coords)
        "toggle"
        (reduce toggle st coords)))))

(defn part-1 []
  (time (let [st (mk-state false 1000 1000)
              commands (->> input
                            (map parse-input)
                            ;(take 4)
                            )
              new-st (reduce (do-cmd turn-off turn-on toggle) st commands)]
          (->> new-st
               flatten
               (filter identity)
               count))))

(defn part-2 []
  (let [st (mk-state 0 1000 1000)
        commands (->> input
                      (map parse-input)
                      ;(take 4)
                      )
        new-st (reduce (do-cmd lower higher much-higher) st commands)]
    (->> new-st
         flatten
         (reduce +))))

(defn x-2 []
  (println (set-value (mk-state false 3 3) [0 0] true)))

(defn x-3 []
  (create-cords [0 0][1 1]))