(ns advent-2015.day12
  (:require [clojure.java.io :as io]
            [utils :as u]
            [clojure.string :as s]
            [clojure.walk :refer [prewalk]]))

(def input (slurp (io/resource "day12")))

(defn ->whole-number [ns]
  (assert (coll? ns))
  (u/string->int (apply str ns)))

(defn num-or-minus? [x]
  (boolean (or (= \- x)
               (u/char->int-not-strict x))))

(defn add-numbers [in]
  (->> in
       (partition-by num-or-minus?)
       ;(drop 8)
       ;(take 10)
       (filter #(-> % first num-or-minus?))
       (map ->whole-number)
       (reduce +)))

(defn part-1 []
  (add-numbers input))

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
