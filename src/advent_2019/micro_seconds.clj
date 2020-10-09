(ns advent-2019.micro-seconds
  (:require
    [clojure.java.io :as io]
    [au.com.seasoft.general.dev :as dev]
    [clojure.string :as str]))

;Problem Statement
;You are writing code for a hardware stopwatch. The device contains an oscillator that works at 100Hz and increments an in-memory integer 100 times per second.
; Thus, after 2 seconds this register will contain the number 200.
;You are to write code (with tests, if you know how) that converts this number into a proper stopwatch display for the user in the following format:
;hh:mm:ss.hh
;Constraints
;Your target is an embedded device environment with low memory constraints.
;The production code can use only str and math operators from clojure core.
;Tests can use anything.

; Whole load of micro-seconds
; How many hours?
; Remainder after take those hours (in micro-seconds) from 'whole load of micro-seconds'
; In this remainder we need to find how many minutes
; Lets do for the hours and make a general function can use for minutes and seconds as well

(defn left-pad-2-zeros [n]
  (let [pad-for (- 2 (-> n str count))]
    (str (str/join (repeat pad-for "0")) n)))

;;
;; Each step to have :format-text and :count-within
;;
(defn micro-seconds-breakdown-2 [total-time steps]
  (loop [remaining-of-micro-unit total-time
         steps steps
         output []]
    (if (empty? steps)
      (str/join output)
      (let [{:keys [format-text count-within]} (first steps)
            how-many (int (/ remaining-of-micro-unit count-within))
            left-over (rem remaining-of-micro-unit count-within)]
        (recur left-over
               (next steps)
               (into output [(left-pad-2-zeros how-many) format-text]))))))

(def num-micro-in-micro 1)
(def num-micro-in-second 100)
(def num-micro-in-min (* 60 num-micro-in-second))
(def num-micro-in-hour (* 60 num-micro-in-min))

(def example-1 (+ (* 3 num-micro-in-hour)
                  (* 2 num-micro-in-min)))

(def example-2 2345532)

(defn x-1 []
  (let [how-many-hours (/ example-1 num-micro-in-hour)
        left-over (rem example-1 num-micro-in-hour)]
    [how-many-hours left-over]))

(defn x-3 []
  (left-pad-2-zeros 0))

(defn micro-seconds-breakdown-1 [total-time]
  (loop [remaining-micro-seconds total-time
         values-undone [num-micro-in-hour num-micro-in-min num-micro-in-second num-micro-in-micro]
         text-undone #_[" hour " " min " " second " " micro "] [":" ":" "." ""]
         output []]
    (if (empty? values-undone)
      (str/join output)
      (let [num-micro-secs-in-unit (first values-undone)
            how-many (int (/ remaining-micro-seconds num-micro-secs-in-unit))
            left-over (rem remaining-micro-seconds num-micro-secs-in-unit)]
        (recur left-over
               (next values-undone)
               (next text-undone)
               (into output [(left-pad-2-zeros how-many) (first text-undone)]))))))

(defn x-2 []
  (micro-seconds-breakdown-1 example-2))

(defn x-3 []
  (let [steps [{:format-text ":"
                :count-within num-micro-in-hour}
               {:format-text ":"
                :count-within num-micro-in-min}
               {:format-text "."
                :count-within num-micro-in-second}
               {:format-text ""
                :count-within num-micro-in-micro}]]
    (micro-seconds-breakdown-2 example-2 steps)))


