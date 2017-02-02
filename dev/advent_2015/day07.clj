(ns advent-2015.day07
  (:require [clojure.java.io :as io]
            [utils :as u]
            [clojure.pprint :as pp]))

(defn input [filename]
  (line-seq (io/reader (io/resource filename))))

(defn parse-connection [line]
  (let [[a b c d e] line
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
       (map parse-connection)))

(def test-parsed-input (into {} (map (juxt :out identity) (parse (input "day07_test")))))
(def real-parsed-input (into {} (map (juxt :out identity) (parse (input "day07")))))

(defn solve-param [solved acc param]
  (if (number? param)
    param
    (some #(when (= (:out %) param) (:value %)) solved)))

(defn calc [cmd params]
  (condp = cmd
    'RSHIFT (apply bit-shift-right params)
    'LSHIFT (apply bit-shift-left params)
    'NOT (bit-and 16rFFFF (bit-not (first params)))
    'AND (apply bit-and params)
    'OR (apply bit-or params)))

;;
;; Sometimes returns an acc with the element that has come in, which was always a part of it, solved.
;; By solved we mean that it has a :value.
;; If can't yet be solved, just return acc. We can get it some future iteration
;;
(defn solve [solved acc ele]
  (let [param-solver (partial solve-param solved acc)
        params (:params ele)
        ]
    (if params
      (let [param-values (map param-solver params)]
        (if (every? (complement nil?) param-values)
          (let [res (calc (:cmd ele) param-values)
                new-ele (assoc ele :value res)]
            (assoc acc (:out ele) new-ele))
          acc))
      (if (and (= (:cmd ele) :assign) (:value ele) (not (number? (:value ele))))
        (let [param-value (param-solver (:value ele))]
          (if param-value
            (let [new-ele (assoc ele :value param-value)]
              (assoc acc (:out ele) new-ele))
            acc))
        (assert false (str "No params nor assign in " ele))))))

(defn solved? [x]
  (number? (:value x)))

(defn finished? [st]
  (->> st
       ;u/probe-on
       vals
       (every? :value)
       ;u/probe-on
       ))

(defn solver [solved]
  (let [my-solver (partial solve solved)]
    (fn [unsolved]
      (reduce my-solver unsolved (->> unsolved vals (remove (fn [x] (solved? x))))))))

(defn next-state [out-map]
  (let [pre-solved (->> out-map vals (filter (fn [x] (solved? x))))
        my-solver (solver pre-solved)]
    (my-solver out-map)))

(defn x-1 []
  (->> (next-state test-parsed-input)
       vals
       (map (juxt :out :value))))

(defn part-1 []
  (->> (drop-while (complement finished?) (iterate next-state real-parsed-input))
       first
       vals
       (map (juxt :out :value))
       (some #(when (= 'a (first %)) %))))
;; => 956

;;
;; Looking at the data b is never an output. Hence assoc is just adding another entry here
;;
(defn part-2-alteration [parsed-input]
  (assoc parsed-input 'b {:cmd :assign :value 956 :out 'b}))

(defn part-2 []
  (->> (drop-while (complement finished?) (iterate next-state (part-2-alteration real-parsed-input)))
       first
       vals
       (map (juxt :out :value))
       (some #(when (= 'a (first %)) %))))

(defn x-2 []
  (take 2 (part-2-alteration real-parsed-input)))




