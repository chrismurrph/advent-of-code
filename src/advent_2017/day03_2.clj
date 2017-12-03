(ns advent-2017.day03-2
  (:require [advent-2017.day03-1 :as first-part]
            [clojure.test :refer :all]))

(def default {[0 0] 1})
(def -pos->value (atom default))

(defn get-value [pos]
  (get @-pos->value pos))

(defn set-value [pos value]
  (swap! -pos->value assoc pos value))

(defn ord->position [ord]
  (-> (first-part/position-info ord)
      first-part/info->answer
      :position))

(def outliers [[0 1] [1 1] [1 0] [1 -1] [0 -1] [-1 -1] [-1 0] [-1 1]])

(defn pos->outliers [pos]
  (reduce
    (fn [acc ele]
      (conj acc (mapv + pos ele)))
    []
    outliers))

(defn calc-position-value! [pos]
  (let [existing-val (get-value pos)]
    (if existing-val
      existing-val
      (let [new-val (->> (pos->outliers pos)
                         (map get-value)
                         (filter some?)
                         (reduce + 0))]
        (set-value pos new-val)
        new-val))))

(defn x-2 []
  (reset! -pos->value default)
  (->> (range)
       (map inc)
       (map ord->position)
       (map calc-position-value!)
       (drop-while #(<= % 325489))
       first))

(deftest get-and-set
  (reset! -pos->value default)
  (set-value [2 2] -30)
  (is (= -30
         (get-value [2 2]))))

(deftest as-per-text
  (reset! -pos->value default)
  (calc-position-value! [1 0])
  (calc-position-value! [1 1])
  (is (= 4
         (calc-position-value! [0 1]))))
