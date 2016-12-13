(ns advent.thirteen
  (:require [utils :as u]
            [clojure.pprint :as pp]))

(defn dec->binary [n]
  (Integer/toString n 2))

;; x*x + 3*x + 2*x*y + y + y*y
(defn coord->wall? [fav-num]
  (fn [[x y]]
    (let [pre-seed (+ (* x x) (* 3 x) (* 2 x y) y (* y y))
          post-seed (+ fav-num pre-seed)
          as-binary (dec->binary post-seed)
          res (reduce + (map u/string->int as-binary))]
      (odd? res))))

(defn create-row [f width y]
  (for [i (range width)
        :let [wall? (f [i y])]]
    (if wall?
      \#
      \.)))

(defn visual-validation [f width height]
  (into [] (for [i (range height)]
             (create-row f width i))))

(defn x []
  (let [num 10
        formula (coord->wall? num)
        ;wall? (formula [0 0])
        ]
    (pp/pprint (visual-validation formula 10 7))))
