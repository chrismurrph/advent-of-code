(ns advent-2015.day13
  (:require [clojure.java.io :as io]
            [utils :as u]))

(def input
  (line-seq (io/reader (io/resource "day13"))))

(defn get-data [in]
  (->> in
       ;(take 8)
       u/probe-off
       (map (partial re-matches #"(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)."))
       (map next)
       (map (juxt first (fn [[_ b c]]
                          (let [sign (if (= "lose" b) "-" "+")]
                            (u/string->int (str sign c)))) u/fourth))))

(def me "Chris")

(defn calc [m one-possible-perm]
  ;(println one-possible-perm)
  (->> one-possible-perm
       (partition 2 1)
       (mapcat (fn [[name-1 name-2]]
                 (if (or (= name-1 me) (= name-2 me))
                   [0 0]
                   [(m [name-1 name-2]) (m [name-2 name-1])])))))

;;
;; Repeating the first at the end will give us the calculation for them sitting next to each other
;;
(defn form-circle [[a & _ :as one-perm]]
  (-> one-perm
      vec
      (conj a)))

(defn max-and-min [x & more]
  (let [maximum (apply max x more)
        minimum (apply min x more)]
    [maximum minimum]))

(defn x-1 []
  (let [data (get-data input)
        m (->> data
               (map (juxt (fn [[a _ c]]
                            [a c]) second))
               (into {}))
        ;_ (u/pp m)
        calculator (partial calc m)
        names (->> data
                   (map first)
                   distinct)
        part-2-names (conj names me)
        _ (println part-2-names)
        perms (u/permutations (conj part-2-names me))
        happinesses (map (comp calculator form-circle) perms)]
    (->> happinesses
         u/probe-off
         (map (partial reduce +))
         u/probe-off
         (apply max-and-min)
         )))
