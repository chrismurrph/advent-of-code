(ns advent-2019.day2
  (:require
    [clojure.java.io :as io]))

(defn add-instruction [[_ input-1 input-2 output] instructions]
  (let [input-1-val (get instructions input-1)
        input-2-val (get instructions input-2)
        result (+ input-1-val input-2-val)]
    (assoc instructions output result)))

(defn multiply-instruction [[_ input-1 input-2 output] instructions]
  (let [input-1-val (get instructions input-1)
        input-2-val (get instructions input-2)
        result (* input-1-val input-2-val)]
    (assoc instructions output result)))

(defn x-1a []
  (add-instruction [1 9 10 3] [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]))

(def op-code->length
  {99 1
   1  4
   2  4})

(def op-code->instruction-f
  {99 nil
   1  add-instruction
   2  multiply-instruction})

(defn computer-a [instructions]
  (loop [instructions instructions counter 0]
    (let [[op-code :as instruction] (->> instructions
                                         (drop (* 4 counter))
                                         (take 4))]
      (if (= 99 op-code)
        instructions
        (let [instruction-f (op-code->instruction-f op-code)
              new-instructions (instruction-f instruction instructions)]
          (recur new-instructions (inc counter)))))))

(defn computer-b [instructions]
  (loop [instructions instructions
         counter 0
         already-done 0]
    (let [yet-to-go (->> instructions
                         (drop already-done))
          [op-code] (take 1 yet-to-go)
          instruction-length (get op-code->length op-code)
          [op-code :as instruction] (->> yet-to-go
                                         (take instruction-length))]
      (if (= 99 op-code)
        instructions
        (let [new-instructions (case op-code
                                 1 (add-instruction instruction instructions)
                                 2 (multiply-instruction instruction instructions))]
          (recur new-instructions
                 (inc counter)
                 (+ already-done instruction-length)))))))

(defn x-2a []
  (computer-a [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]))

(def example-1 [1, 0, 0, 0, 99])
(def example-2 [2, 3, 0, 3, 99])
(def example-3 [2, 4, 4, 5, 99, 0])
(def example-4 [1, 1, 1, 4, 99, 5, 6, 0, 99])

(defn x-3a []
  (computer-a example-4))

(defn solve-a []
  (let [as-str (slurp (io/resource "day2.edn"))
        instructions (->> (read-string (str "[" as-str "]"))
                          (#(-> %
                                (assoc 1 12)
                                (assoc 2 2))))
        after-run (computer-a instructions)]
    (get after-run 0)))

(comment
  "Answer part A"
  (solve-a))

(defn instructions! []
  (read-string (str "[" (slurp (io/resource "day2.edn")) "]")))

(defn solve-b [[input-1 input-2]]
  (let [instructions (->> (instructions!)
                          (#(-> %
                                (assoc 1 input-1)
                                (assoc 2 input-2))))
        altered-instructions (computer-b instructions)]
    (get altered-instructions 0)))

(defn x-1b
  "Check they work the same"
  []
  [(solve-a) (solve-b [12 2])])

(defn calc-possible-inputs []
  (for [x1 (range 100)
        x2 (range 100)]
    [x1 x2]))

(defn x-2b []
  (let [inputs (calc-possible-inputs)
        [noun verb :as answer-input] (some #(when (= 19690720 (solve-b %)) %) inputs)]
    (+ verb (* 100 noun))))

(comment
  "Answer part B"
  (x-2b))


