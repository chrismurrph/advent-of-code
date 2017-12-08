(ns advent-2017.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn get-example-input []
  (->> (io/resource "2017/day08_example")
       slurp
       s/split-lines))

(defn get-input []
  (->> (io/resource "2017/day08")
       slurp
       s/split-lines))

;; b inc 5 if a > 1
(def instruction-regex #"(\S+) (\S+) (-?\d+) if (\S+) (\S+) (-?\d+)")

(defn make-instruction [[register op amount if-register comp num]]
  (cond-> {:register    register
           :op          op
           :if-register if-register
           :comp        comp}
          amount (assoc :amount (Integer/parseInt amount))
          num (assoc :num (Integer/parseInt num))))

(defn parse [line]
  (->> (re-find instruction-regex line)))

(def op-name->fn
  {"inc" +
   "dec" -})

(defn operation [op amount]
  (fn [old-register-value]
    (let [old-register-value (or old-register-value 0)]
      ((op-name->fn op) old-register-value amount))))

(def maximum (fnil max Integer/MIN_VALUE))

(def comp->fn
  {">"  >
   "<"  <
   ">=" >=
   "<=" <=
   "==" =
   "!=" not=})

;;
;; memory is just a map of register -> value
;;
(defn process-instruction [[memory
                            [{:keys [register op amount if-register comp num]} & rest-instructions]
                            kept-highest]]
  (let [update-register-f (operation op amount)
        new-memory (cond-> memory

                           ((comp->fn comp) (or (get memory if-register) 0) num)
                           (update register update-register-f))
        new-highest (apply maximum (vals new-memory))
        new-kept-highest (maximum kept-highest new-highest)
        ]
    [new-memory rest-instructions new-kept-highest]))

(defn ending-state? [[_ instructions _]]
  (empty? instructions))

(defn iterations [input]
  (let [in (->> input
                (map parse)
                (map next)
                (map make-instruction)
                )]
    (->> (iterate process-instruction [{} in])
         (drop-while (complement ending-state?)))))

;; ans: 5946
(defn x-1 []
  (->> (iterations (get-input))
       ffirst
       vals
       (apply max)))

;; ans: 6026
(defn x-2 []
  (->> (iterations (get-input))
       first
       (drop 2)
       first))

