(ns advent-2015.day09
  (:require [clojure.java.io :as io]
            [utils :as u]))

(def input
  (line-seq (io/reader (io/resource "day09"))))

(def places ["Arbre" "Straylight" "Tristram" "Snowdin" "Tambi" "Norrath" "AlphaCentauri" "Faerun"])
(println (str "Num places: " (count places)))
(def perms (u/permutations places))
(println (str "Num perms: " (count perms)))

(def distances
  (->> input
       (map #(str "[" % "]"))
       (map read-string)
       (map (juxt (fn [v] (set ((juxt (comp str first) (comp str u/third)) v))) u/fifth))
       (into {})))
(println (str "first distance " (first (keys distances))))
(println (str "Combos: " (count (seq (u/combinations places 2)))))

(defn total-distance [idx places]
  (->> places
       (partition 2 1)
       (map set)
       (map distances)
       (apply +)
       (vector idx)))

(defn least-distance [[_ acc-dist :as acc] [_ ele-dist :as ele]]
  (if (< ele-dist acc-dist) ele acc))

(defn greatest-distance [[_ acc-dist :as acc] [_ ele-dist :as ele]]
  (if (> ele-dist acc-dist) ele acc))

(defn x-1 []
  (let [[smallest dist] (->> perms
                      ;(take 3)
                      (map-indexed total-distance)
                      (reduce least-distance))
        best-route (nth perms smallest)]
    dist))

(defn x-2 []
  (let [[biggest dist] (->> perms
                             ;(take 3)
                             (map-indexed total-distance)
                             (reduce greatest-distance))
        best-route (nth perms biggest)]
    dist))