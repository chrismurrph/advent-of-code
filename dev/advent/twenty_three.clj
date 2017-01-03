(ns advent.twenty-three
  (:require [instaparse.core :as insta])
  (:import (java.io BufferedReader StringReader)))

(def grammar-1
  (insta/parser
    "<S> = cmd
     cpy = <'cpy'>
     inc = <'inc'>
     dec = <'dec'>
     jnz = <'jnz'>
     tgl = <'tgl'>
     <inst> = cpy | inc | dec | jnz | tgl
     int = #'[-0-9]+'
     <var> = 'a' | 'b' | 'c' | 'd'
     <arg> = int | var
     cmd = inst <' '> arg (<' '> arg) ?
     "
    ))

(defn to-hiccup [s]
  (->> (first (grammar-1 s))
       (insta/transform
         {:int clojure.edn/read-string
          :cpy (fn [] :cpy)
          :inc (fn [] :inc)
          :dec (fn [] :dec)
          :jnz (fn [] :jnz)
          :tgl (fn [] :tgl)})))

(defn retrieve-instructions [steps]
  (let [instructions (map to-hiccup steps)]
    (mapv (comp vec next) instructions)))

(def test-tags {"a" 0 "b" 0 "c" 0 "d" 0})
(def real-tags {"a" 7 "b" 0 "c" 0 "d" 0})
(def part-two-tags {"a" 12 "b" 0 "c" 0 "d" 0})

(defn inc-i [[tag] tags]
  ;(println (str "inc of " tag))
  (update tags tag inc))

(defn mul-i [[tag] tags]
  ;(println (str "inc of " tag))
  (update tags tag (fn [n] (*' n 1))))

(defn dec-i [[tag] tags]
  ;(println (str "dec of " tag))
  (update tags tag dec))

(defn cpy-i [[num tag] tags]
  (let [value (if (number? num) num (get tags num))]
    ;(println (str "cpy of " tag))
    (assoc tags tag value)))

(def instr-functions {:cpy cpy-i
                      :dec dec-i
                      :inc inc-i})

(def toggle-instructions {:jnz :cpy
                          :tgl :inc
                          :inc :dec
                          :cpy :jnz})

(defn return-args
  ([one two]
   [one two])
  ([one]
    [one]))

(defn runner [instructs]
  (println instructs)
  (assert (vector? instructs))
  (fn [tags]
    (loop [state tags
           instructions instructs
           index 0
           counted 0
           toggle-from-idx nil]
      (let [
            [instr & args] (get instructions index)
            ;_ (println "at idx" index "instr" instr)
            ]
        (if toggle-from-idx
          (if (>= index (count instructions))
            (recur state instructions (inc toggle-from-idx) (inc counted) nil)
            (let [_ (println (str "From " toggle-from-idx ", brought us to " index))
                  new-instr (toggle-instructions instr)
                  _ (assert new-instr (str "No toggle instruction found for " instr))
                  ;f (instr-functions new-instr)
                  ;_ (assert f (str "No function for: " new-instr ", from " instr))
                  new-instructions (assoc instructions index (into [new-instr] (apply return-args args)))
                  _ (println "new-instructions: " new-instructions)
                  ]
              (recur state new-instructions (inc toggle-from-idx) (inc counted) nil)))
          (if instr
            (cond
              (= :jnz instr)
              (if (and (not (zero? (get state "c"))) (= index 7) (= -2 (second args)))
                (let [new-state (update state "a" (fn [old-a]
                                                    (+ old-a (get state "c"))))]
                  (recur new-state instructions (inc index) (inc counted) nil))
                (if (and (not (zero? (get state "d"))) (= index 15) (= -2 (second args)))
                  (let [new-state (update state "c" (fn [old-c]
                                                      (+ old-c (get state "d"))))]
                    (recur new-state instructions (inc index) (inc counted) nil))
                  (let [[tag-a tag-b] args
                        tag-a-value (if (number? tag-a) tag-a (get state tag-a))
                        tag-b-value (if (number? tag-b) tag-b (get state tag-b))
                        _ (assert tag-a-value (str "No tag value found for " tag-a))
                        _ (assert tag-b-value (str "No tag value found for " tag-b))
                        jump-by (if (zero? tag-a-value) 1 tag-b-value)
                        new-idx (+ index jump-by)]
                    (recur state instructions new-idx (inc counted) nil))))

              (= :tgl instr)
              (let [[tag] args
                    tag-value (if (number? tag) tag (get state tag))
                    _ (assert tag-value (str "No tag value found for " tag))
                    new-idx (+ index tag-value)
                    ;_ (println new-idx)
                    ]
                (recur state instructions new-idx (inc counted) index))

              :default
              (let [f (instr-functions instr)
                    _ (assert f (str "No function for: " instr))
                    new-state (f args state)]
                (recur new-state instructions (inc index) (inc counted) nil)))
            state))))))

(def real-file "./advent/twenty_three.txt")
(def test-file "./advent/twenty_three_example.txt")
(def file-name real-file)

(defn x []
  (let [raw-input (slurp file-name)
        tags part-two-tags
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        instructions (retrieve-instructions in)
        run (runner instructions)]
    (run tags)))
