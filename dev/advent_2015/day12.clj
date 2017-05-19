(ns advent-2015.day12
  (:require [clojure.java.io :as io]
            [utils :as u]
            [clojure.string :as s]
            [clojure.walk :refer [prewalk]]))

(def input (slurp (io/resource "day12")))

(defn xs->whole-number [ns]
  (assert (coll? ns))
  (u/string->int (apply str ns)))

(defn digit-or-minus? [x]
  (boolean (or (= \- x)
               (u/char->int? x))))

(defn add-numbers [in]
  (->> in
       u/probe-off
       (partition-by digit-or-minus?)
       u/probe-off
       (filter #(-> % first digit-or-minus?))
       u/probe-off
       (map xs->whole-number)
       u/probe-off
       (reduce +)))

(defn add-numbers-2 [in]
  (->> in
       (re-seq #"-?\d+")
       u/probe-off
       (map read-string)
       (reduce +)))
;;=> 111754

(defn add-numbers-3 [in]
  (->> in
       (re-seq #"\d+")
       u/probe-on
       (map read-string)
       (reduce +)))

(defn part-1 []
  (add-numbers-2 input))

(defn contains-red? [x]
  (and (map? x)
       (->> x
            vals
            (some #(when (= (str %) "red") %)))))

(defn part-2 []
  (->> (s/replace input ":" " ")
       read-string
       (prewalk #(if (contains-red? %) nil %))
       str
       add-numbers))

(defn x-1 []
  (add-numbers-2 "12abc12"))
