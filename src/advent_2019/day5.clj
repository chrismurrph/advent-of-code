(ns advent-2019.day5
  (:require
    [advent-of-code.day-5-instructions :as instructions]
    [clojure.java.io :as io]
    [au.com.seasoft.general.dev :as dev]))

(def op-code->length-m {1 4
                        2 4
                        3 2
                        4 2
                        5 3
                        6 3
                        7 4
                        8 4})

(defn op-code->length [op-code]
  (assert op-code "No op-code")
  (let [len (get op-code->length-m (instructions/last-digit op-code))]
    (when (nil? len)
      (dev/err "Not supported, length of op-code for op-code" op-code))
    len))

(def op-code->instruction-m {1 instructions/add-instruction-2
                             2 instructions/multiply-instruction
                             3 instructions/input-instruction
                             4 instructions/output-instruction
                             5 instructions/jump-if-true
                             6 instructions/jump-if-false
                             7 instructions/is-less-than
                             8 instructions/is-equals
                             })

(defn op-code->instruction-f [op-code]
  (let [len (or (get op-code->instruction-m op-code) (get op-code->instruction-m (instructions/last-digit op-code)))]
    (when (nil? len)
      (dev/err "Not supported, instruction of op-code" op-code))
    len))

;;
;; instructions get replaced but never added or removed
;; inputs just stay in the loop until an input instruction comes along, then just take first so rest loop.
;; outputs - here an empty vector that conj onto the end of. Pass it back with the instructions at the end of
;; the run (when 99).
;; The input instruction will have a different signature.
;; The output instruction will return a vector, and 2nd thing will be conj-ed onto outputs.
;;
(defn computer [instructions inputs]
  (loop [instructions instructions
         inputs inputs
         outputs []
         already-done 0]
    (assert (int? already-done))
    (let [yet-to-go (drop already-done instructions)
          [op-code] (take 1 yet-to-go)]
      (if (or (nil? op-code) (= 99 op-code))
        [instructions outputs]
        (let [instruction-length (op-code->length op-code)
              [op-code :as instruction] (take instruction-length yet-to-go)
              _ (dev/log-off "instruction" instruction)
              instruction-f (op-code->instruction-f op-code)
              short-op-code (instructions/last-digit op-code)]
          (assert instructions ["No instructions after" op-code])
          (if (= 4 short-op-code)
            (let [output-value (instruction-f instruction instructions)]
              (recur instructions
                     inputs
                     (conj outputs output-value)
                     (+ already-done instruction-length)))
            (if (#{5 6} short-op-code)
              (let [new-already-done (instruction-f instruction instructions)]
                (recur instructions
                       inputs
                       outputs
                       (or new-already-done (+ already-done instruction-length))))
              (let [new-instructions (case short-op-code
                                       3 (instruction-f instruction instructions (first inputs))
                                       (instruction-f instruction instructions))]
                (recur new-instructions
                       (if (= 3 short-op-code)
                         (next inputs)
                         inputs)
                       outputs
                       (+ already-done instruction-length))))))))))

(defn day5-instructions! []
  (read-string (str "[" (slurp (io/resource "day5.edn")) "]")))

(defn x-2a []
  (let [inputs [5 6]
        instructions [3, 0, 4, 0, 99]
        [new-instructions outputs] (computer instructions inputs)]
    (dev/log-on outputs)))

(defn x-3a []
  (let [inputs []
        instructions [104 0] #_[1101, 100, -1, 4, 0] #_[1002, 4, 3, 4, 33]
        [new-instructions outputs] (computer instructions inputs)]
    new-instructions))

(defn solve-a
  "This solves it, the last output is correct: 13787043"
  []
  (let [inputs [1]
        instructions (day5-instructions!)
        [new-instructions outputs] (computer instructions inputs)]
    outputs))

(comment
  (take 16 day5-instructions)
  (= 13787043 (last (solve-a))))

;;
;; Below 8 s/give 999 but gives nil.
;;
(defn x-4a []
  (let [inputs [7]
        instructions [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
        [new-instructions outputs] (computer instructions inputs)]
    outputs))

(defn solve-b
  ""
  []
  (let [inputs [5]
        instructions (day5-instructions!)
        [new-instructions outputs] (computer instructions inputs)]
    outputs))

(comment
  (solve-b))
