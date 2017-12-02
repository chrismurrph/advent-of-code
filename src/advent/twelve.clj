(ns advent.twelve
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io])
  (:import (java.io BufferedReader StringReader)))

(def steps ["cpy 41 a"
            "inc a"
            "inc a"
            "dec a"
            "jnz a 2"
            "dec a"])

(def grammar-1
  (insta/parser
    "<S> = cmd
     cpy = <'cpy'>
     inc = <'inc'>
     dec = <'dec'>
     jnz = <'jnz'>
     <inst> = cpy | inc | dec | jnz
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
          :jnz (fn [] :jnz)})))

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

(defn runner [instructions]
  (println instructions)
  (assert (vector? instructions))
  (fn [tags]
    (loop [state tags
           index 0
           count 0]
      (let [
            ;_ (println "at idx " index)
            [instr & args] (get instructions index)]
        (if instr
          (if (= :jnz instr)
            (let [[tag num] args
                  tag-value (if (number? tag) tag (get state tag))
                  _ (assert tag-value (str "No tag value found for " tag))
                  jump-by (if (zero? tag-value) 1 num)
                  new-idx (+ index jump-by)]
              (recur state new-idx (inc count)))
            (let [f (instr-functions instr)
                  _ (assert f (str "No function for: " instr))
                  new-state (f args state)]
              (recur new-state (inc index) (inc count))))
          state)))))

(defn x-1 []
  (let [instructions (retrieve-instructions steps)
        run (runner instructions)]
    (run tags)))

(defn x []
  (let [raw-input (slurp (io/resource "2016/twelve.txt"))
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        instructions (retrieve-instructions in)
        run (runner instructions)]
    (run tags)))
