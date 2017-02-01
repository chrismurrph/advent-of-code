(ns advent-2015.day07
  (:require [clojure.java.io :as io]
            [utils :as u]
            [clojure.pprint :as pp]))

(defn input [filename]
  (line-seq (io/reader (io/resource filename))))

(defn parse-connection [line]
  (let [
        ;_ (println line)
        [a b c d e] line
        cmd (first (filter #{'RSHIFT 'LSHIFT 'NOT 'AND 'OR} line))]
    (if cmd
      (condp = cmd
        'RSHIFT {:cmd 'RSHIFT :params [a c] :out e}
        'LSHIFT {:cmd 'LSHIFT :params [a c] :out e}
        'NOT {:cmd 'NOT :params [b] :out d}
        'AND {:cmd 'AND :params [a c] :out e}
        'OR {:cmd 'OR :params [a c] :out e})
      {:cmd :assign :value a :out c})))

(defn parse [input]
  (->> input
       (map #(str "[" % "]"))
       (map read-string)
       u/probe-off
       (map parse-connection)
              ;(take 4)
       ))

(def test-parsed-input (into {} (map (juxt :out identity) (parse (input "day07_test")))))
(def real-parsed-input (into {} (map (juxt :out identity) (parse (input "day07")))))

(defn solve-param [solved acc param]
  (let [res (if (number? param) param (some #(when (= (:out %) param) (:value %)) solved))]
    (assert res (str "Not yet solved (maybe use acc) " param))
    res))

(defn calc [cmd params]
  (condp = cmd
    'RSHIFT (apply bit-shift-right params)
    'LSHIFT (apply bit-shift-left params)
    'NOT (bit-and 16rFFFF (bit-not (first params)))
    'AND (apply bit-and params)
    'OR (apply bit-or params)))

;;
;; Always returns an acc with the element that has come in, which was always a part of it, solved.
;; By solved we mean that it has a :value.
;;
(defn solve [solved acc ele]
  (let [param-solver (partial solve-param solved acc)
        ;_ (println ele)
        params (:params ele)
        _ (assert params (str "No params in " ele))
        param-values (map param-solver params)
        _ (assert (every? (complement nil?) param-values))
        res (calc (:cmd ele) param-values)
        _ (assert res (str ele ", " (seq param-values) ", " res))
        new-ele (assoc ele :value res)
        ]
    (assoc acc (:out ele) new-ele)))

(defn solved? [x]
  (:value x))

(defn finished? [st]
  (->> st
       vals
       u/probe-on
       (every? :value)
       u/probe-on))

(defn solver [solved]
  (let [my-solver (partial solve solved)]
    (fn [unsolved]
      (reduce my-solver unsolved (->> unsolved vals (remove (fn [x] (solved? x))))))))

(defn next-state [out-map]
  (let [pre-solved (->> out-map vals (filter (fn [x] (solved? x))))
        my-solver (solver pre-solved)
        ]
    (my-solver out-map)))

(defn x-1 []
  (->> (next-state test-parsed-input)
       vals
       (map (juxt :out :value))
       ))

(defn x-2 []
  (->> (drop-while (complement finished?) (iterate next-state real-parsed-input))
       first
       vals
       (map (juxt :out :value))
       ))


