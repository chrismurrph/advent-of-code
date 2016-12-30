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

(def tags {"a" 0 "b" 0 "c" 1 "d" 0})

(defn inc-i [[tag] tags]
  ;(println (str "inc of " tag))
  (update tags tag inc))

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

(def toggle-instructions {:tgl :inc
                          :inc :dec})

(defn runner [instructs]
  (println instructs)
  (assert (vector? instructs))
  (fn [tags]
    (loop [state tags
           instructions instructs
           index 0
           count 0
           toggle-from-idx nil]
      (let [
            ;_ (println "at idx " index)
            [instr & args] (get instructions index)]
        (if toggle-from-idx
          (let [new-instr (toggle-instructions instr)
                f (instr-functions new-instr)
                _ (assert f (str "No function for: " new-instr))
                ]
            (recur state instructions (inc index) (inc count) nil))
          (if instr
            (cond
              (= :jnz instr)
              (let [[tag num] args
                    tag-value (if (number? tag) tag (get state tag))
                    _ (assert tag-value (str "No tag value found for " tag))
                    jump-by (if (zero? tag-value) 1 num)
                    new-idx (+ index jump-by)]
                (recur state instructions new-idx (inc count) nil))

              (= :tgl instr)
              (let [[tag] args
                    tag-value (if (number? tag) tag (get state tag))
                    _ (assert tag-value (str "No tag value found for " tag))
                    new-idx (+ index tag-value)
                    _ (println new-idx)
                    ]
                (recur state instructions new-idx (inc count) index))

              :default
              (let [f (instr-functions instr)
                    _ (assert f (str "No function for: " instr))
                    new-state (f args state)]
                (recur new-state instructions (inc index) (inc count) nil)))
            state))))))

(def real-file "./advent/twenty_three.txt")
(def test-file "./advent/twenty_three_example.txt")
(def file-name test-file)

(defn x []
  (let [raw-input (slurp file-name)
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        instructions (retrieve-instructions in)
        run (runner instructions)]
    (run tags)))
