(ns advent-2017.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [utils :as u]))

(defn get-example-input []
  (->> (io/resource "2017/day08_example")
       slurp
       s/split-lines))

(defn get-input []
  (->> (io/resource "2017/day08")
       slurp
       s/split-lines))

(def maximum u/maximum)

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
  (fn [register-value]
    ((op-name->fn op) register-value amount)))

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
  (let [new-memory (cond-> memory

                           ((comp->fn comp) (or (get memory if-register) 0) num)
                           (update register (fnil (operation op amount) 0)))
        new-highest (apply maximum (vals new-memory))
        new-kept-highest (maximum kept-highest new-highest)
        ]
    [new-memory rest-instructions new-kept-highest]))

(defn ending-state? [[_ instructions _]]
  (empty? instructions))

(def x-create-instruction
  (comp
    (map parse)
    (map next)
    (map make-instruction)))

(defn iterations [input]
  (let [in (->> input
                (sequence x-create-instruction))]
    (->> (iterate process-instruction [{} in])
         (drop-while (complement ending-state?)))))

;; ans: 5946
(defn x-1 []
  (->> (iterations (get-input))
       ffirst
       vals
       (apply maximum)))

;; ans: 6026
(defn x-2 []
  (->> (iterations (get-input))
       first
       (drop 2)
       first))

