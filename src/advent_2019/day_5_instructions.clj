(ns advent-2019.day-5-instructions
  (:require [au.com.seasoft.general.dev :as dev]))

(defn to-int [s]
  (Long/parseLong s))

(defn fill-out [required-length fill-value existing-value]
  (assert (vector? existing-value) ["Not vector" existing-value])
  (let [diff (- required-length (count existing-value))
        _ (assert (> diff -1))
        fill (repeat diff fill-value)]
    (into existing-value fill)))

;;
;; 0 is the default mode
;; We return modes as vector of ints, one for each parameter
;; With no other info all are zero
;; We reverse all but the last two
;; BUG HERE
;; FIX - instead of taking fill out with repeat
;;
(defn op-code->modes [op-code num-params dbg?]
  (let [op-code-str (str op-code)
        op-code-length (count op-code-str)]
    (if (= 1 op-code-length)
      (let [res (vec (repeat num-params 0))]
        (when dbg?
          (dev/log-on op-code res))
        res)
      (let [res (->> op-code-str
                     (take (- op-code-length 2))
                     reverse
                     (map (comp to-int str))
                     vec
                     (fill-out num-params 0))]
        (when dbg?
          (dev/log-on op-code res))
        res))))

(defn read-value-according-mode [instructions param mode]
  (case mode
    1 param
    0 (get instructions param)
    (dev/err "mode just 0 or 1:" mode)))

(defn params->values-1 [[op-code & params :as instruction] param-count instructions]
  (let [modes (op-code->modes op-code param-count false)
        param-values (->> params
                          (take param-count)
                          (map-indexed (fn [idx param]
                                         (read-value-according-mode instructions param (nth modes idx)))))]
    param-values))

;;
;; Only param-count params are involved in the reading mode. So we are careful to not do the others, but
;; put them on the end.
;; Takes an instruction (plus other params) and returns an instruction
;;
(defn params->values-2 [[op-code & params :as instruction] param-count instructions]
  (let [modes (op-code->modes op-code param-count false)
        param-values (->> params
                          (take param-count)
                          (map-indexed (fn [idx param]
                                         (read-value-according-mode instructions param (nth modes idx)))))]
    (into (into [op-code] param-values) (drop param-count params))))

(defn last-digit [op-code]
  (assert ((some-fn pos-int? zero?) op-code) ["Problem op-code" op-code])
  (-> op-code str last str to-int))

(defn add-instruction-1 [[op-code param-1 param-2 output] instructions]
  (assert (= 1 (last-digit op-code)) ["Doing add-instruction with wrong op-code" op-code])
  (let [modes (op-code->modes op-code 2 false)
        param-1-val (read-value-according-mode instructions param-1 (nth modes 0))
        param-2-val (read-value-according-mode instructions param-2 (nth modes 1))
        result (+ param-1-val param-2-val)]
    (assoc instructions output result)))

(defn add-instruction-2 [[op-code param-1 param-2 output :as instruction] instructions]
  (assert (= 1 (last-digit op-code)) ["Doing add-instruction with wrong op-code" op-code])
  (let [result (apply + (params->values-1 instruction 2 instructions))]
    (assoc instructions output result)))

(defn multiply-instruction [[op-code param-1 param-2 output :as instruction] instructions]
  (assert (= 2 (last-digit op-code)) ["Doing multiply-instruction with wrong op-code" op-code])
  (let [result (apply * (params->values-1 instruction 2 instructions))]
    (assoc instructions output result)))

(defn input-instruction [[op-code param-1] instructions input]
  (assert (= 3 (last-digit op-code)) ["Doing input-instruction with wrong op-code" op-code])
  (assert input "No input left to consume")
  (assoc instructions param-1 input))

;;
;; Opcode 4 outputs the value of its only parameter.
;; For example, the instruction 4,50 would output the value at address 50.
;;
(defn output-instruction [[op-code param-1 :as instruction] instructions]
  (assert (= 4 (last-digit op-code)) ["Doing output-instruction with wrong op-code" op-code])
  (let [param-values (params->values-1 instruction 1 instructions)]
    (first param-values)))

(defn x-1a []
  (add-instruction-2 [1 9 10 3] [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]))

;;
;; Jump instructions return where to set the instruction pointer, which is the value of already-done
;;

;;
;; Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer
;; to the value from the second parameter. Otherwise, it does nothing.
;;
(defn jump-if-true [[op-code param-1 param-2 :as instruction] instructions]
  (let [params (params->values-1 instruction 2 instructions)]
    (when (-> params first zero? not)
      (second params))))

;;
;; Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from
;; the second parameter. Otherwise, it does nothing.
;;
(defn jump-if-false [[op-code param-1 param-2 :as instruction] instructions]
  (let [params (params->values-1 instruction 2 instructions)]
    (when (-> params first zero?)
      (second params))))

;;
;; Opcode 7 is less than: if the first parameter is less than the second parameter,
;; it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
;;
(defn is-less-than [[op-code param-1 param-2 output :as instruction] instructions]
  (let [params (params->values-1 instruction 2 instructions)]
    (if (< (first params) (second params))
      (assoc instructions output 1)
      (assoc instructions output 0))))

;;
;; Opcode 8 is equals: if the first parameter is equal to the second parameter,
;; it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
;;
(defn is-equals [[op-code param-1 param-2 output :as instruction] instructions]
  (let [params (params->values-1 instruction 2 instructions)]
    (if (= (first params) (second params))
      (assoc instructions output 1)
      (assoc instructions output 0))))