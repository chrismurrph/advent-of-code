(ns advent-2017.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [utils :as u]))

(defn get-example-input []
  (->> (io/resource "2017/day08_example")
       slurp
       s/split-lines
       ))

(defn get-input []
  (->> (io/resource "2017/day08")
       slurp
       s/split-lines
       ))

;; b inc 5 if a > 1
(def regex-1 #"(\S+) (\S+) (-?\d+) if (\S+) (\S+) (-?\d+)")

(defn make-instruction [[register op amount if-register comp num]]
  (cond-> {:register    register
           :op          op
           :if-register if-register
           :comp        comp
           }
          amount (assoc :amount (Integer/parseInt amount))
          num (assoc :num (Integer/parseInt num))
          ))

(defn parse [line]
  (->> (re-find regex-1 line)))

(defn operation [op amount]
  (fn [old-register-value]
    (let [old-register-value (or old-register-value 0)]
      (condp = op
        "inc" (+ old-register-value amount)
        "dec" (- old-register-value amount)
        ))))

;;
;; memory is just a map of register -> value
;;
(defn process-instruction [[memory instructions]]
  (let [{:keys [register op amount if-register comp num]} (first instructions)
        if-register-value (or (get memory if-register) 0)
        cond-triggered? (condp = comp
                          ">" (> if-register-value num)
                          "<" (< if-register-value num)
                          ">=" (>= if-register-value num)
                          "<=" (<= if-register-value num)
                          "==" (= if-register-value num)
                          "!=" (not= if-register-value num)
                          )
        ;_ (println "cond-triggered?" cond-triggered?)
        new-memory (cond-> memory
                           cond-triggered? (update register #((operation op amount) %)))
        ]
    [new-memory (rest instructions)]))

(defn ending-state? [[memory instructions]]
  (empty? instructions))

(defn x-1 []
  (let [in (->> (get-input)
                (map parse)
                (map next)
                dev/probe-off
                (map make-instruction)
                )]
    ;(dev/pp in)
    (->> (iterate process-instruction [{} in])
         (drop-while (complement ending-state?))
         ffirst
         vals
         (apply max))))

