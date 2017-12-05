(ns advent-2017.day01
  (:require [clojure.java.io :as io]))

(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

(defn char->int [ch]
  ((comp #(Integer/parseInt %) str) ch))

(defn split-in-half [xs]
  (let [half-way (/ (count xs) 2)]
    [(take half-way xs) (drop half-way xs)]))

(defn x-1 []
  (let [input (slurp (io/resource "2017/day01"))
        f-conj (flip conj)]
    (->> input
         (mapv char->int)
         (f-conj (-> input first char->int))
         (partition 2 1)
         (filter #(apply = %))
         (map first)
         (reduce +))))

(defn x-2 []
  (let [input (slurp (io/resource "2017/day01"))]
    (->> input
         (mapv char->int)
         split-in-half
         (apply map (fn [x y]
                 (when (= x y)
                   (+ x y))))
         (filter some?)
         (reduce + 0))))
