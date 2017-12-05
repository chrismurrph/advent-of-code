(ns advent-2017.day05
  (:require [clojure.string :as s]
            [clojure.java.io :as io]))

(defn get-input []
  (->> (io/resource "2017/day05")
       slurp
       s/split-lines
       (mapv #(Integer/parseInt %))
       ))

(defn finished-hof? [size]
  (fn [[n _ _]]
    (or (neg? n) (>= n size))))

;;
;; n is where are in instructions, with 0 being first instruction
;; Examine content at n which will inc that contents and change n for next time
;;
(defn transition-1 [[n instructions step]]
  (let [content (get instructions n)
        new-instructions (update instructions n inc)
        new-n (+ n content)]
    [new-n new-instructions (inc step)]))

(defn solve [transition-f]
  (let [in #_[0 3 0 1 -3] (get-input)
        finished? (finished-hof? (count in))
        start-state [0 in 0]]
    (->> (iterate transition-f start-state)
         (drop-while (complement finished?))
         first)))

;; ans: 326618
(defn x-1 []
  (solve transition-1))

;;
;; Now, the jumps are even stranger: after each jump,
;; if the offset was three or more, instead decrease it by 1.
;; Otherwise, increase it by 1 as before.
;;
(defn transition-2 [[n instructions step]]
  (let [content (get instructions n)
        mutation (if (>= content 3) dec inc)
        new-instructions (update instructions n mutation)
        new-n (+ n content)]
    [new-n new-instructions (inc step)]))

;; ans: 21841249
(defn x-2 []
  (solve transition-2))
