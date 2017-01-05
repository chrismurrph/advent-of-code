(ns advent.twenty-five
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io])
  (:import (java.io BufferedReader StringReader)))

(def grammar-1
  (insta/parser
    "<S> = cmd
     cpy = <'cpy'>
     inc = <'inc'>
     dec = <'dec'>
     jnz = <'jnz'>
     out = <'out'>
     sml = <'sml'>
     <inst> = cpy | inc | dec | jnz | out | sml
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
          :out (fn [] :out)
          :sml (fn [] :sml)
          })))

(defn retrieve-instructions [steps]
  (let [instructions (map to-hiccup steps)]
    (mapv (comp vec next) instructions)))

;(def starting-a (atom 0))

(def initial-tags {"a" 2555 "b" 0 "c" 0 "d" 0 :output [] :index 0})

(defn inc-i [[tag] tags]
  ;(println (str "inc of " tag))
  (update tags tag inc))

;(def expected-b (atom nil))
;(def stream-count (atom 0))

(defn out-i [[tag] tags]
  (assert (= tag "b") tag)
  (let [curr-b (get tags tag)]
    (update tags :output conj curr-b)))

(defn fin-i [_ tags]
  (assoc tags :finish true))

(defn dec-i [[tag] tags]
  ;(println (str "dec of " tag))
  (update tags tag dec))

(defn cpy-i [[num tag] tags]
  (let [value (if (number? num) num (get tags num))]
    ;(println (str "cpy of " tag))
    (assoc tags tag value)))

(defn small-program-i [_ tags]
  (let [b (get tags "b")
        b-odd? (odd? b)
        modified-b (if b-odd? (dec b) b)
        halved (/ modified-b 2)
        new-c (if b-odd? 1 2)]
    (assoc tags "a" halved "c" new-c "b" 0)))

(def instr-functions {:cpy cpy-i
                      :dec dec-i
                      :inc inc-i
                      :out out-i
                      :fin fin-i
                      :sml small-program-i})

(defn runner [instructions]
  ;(println instructions)
  (assert (vector? instructions))
  (fn [tags]
    ;(println "starting tags: " tags)
    (let [res (loop [state tags
                     counted 0]
                (if (:finish state)
                  state
                  (let [
                        [instr & args] (get instructions (:index state))
                        ;_ (println "at idx" index "instr" instr)
                        ]
                    (when instr
                      (cond
                        (= :jnz instr)
                        (let [[tag-a tag-b] args
                              tag-a-value (if (number? tag-a) tag-a (get state tag-a))
                              tag-b-value (if (number? tag-b) tag-b (get state tag-b))
                              _ (assert tag-a-value (str "No tag value found for " tag-a " in <" state ">"))
                              _ (assert tag-b-value (str "No tag value found for " tag-b))
                              jump-by (if (zero? tag-a-value) 1 tag-b-value)
                              new-idx (+ (:index state) jump-by)]
                          (recur (assoc state :index new-idx) (inc counted)))

                        :default
                        (let [f (instr-functions instr)
                              _ (assert f (str "No function for: " instr))
                              new-state (f args state)
                              new-idx (inc (:index state))]
                          (recur (assoc new-state :index new-idx) (inc counted))))))))
          ;_ (println "starting-a:" @starting-a)
          ]
      res)))

(defn run-prog [instructions start-state]
  (take-while (comp not nil?)
              (iterate (fn [{:keys [index] :as state}]
                         (let [
                               [instr & args] (get instructions index)
                               ;_ (println "at idx" index "instr" instr)
                               ]
                           (when instr
                             (cond
                               (= :jnz instr)
                               (let [[tag-a tag-b] args
                                     tag-a-value (if (number? tag-a) tag-a (get state tag-a))
                                     tag-b-value (if (number? tag-b) tag-b (get state tag-b))
                                     _ (assert tag-a-value (str "No tag value found for " tag-a " in <" state ">"))
                                     _ (assert tag-b-value (str "No tag value found for " tag-b))
                                     jump-by (if (zero? tag-a-value) 1 tag-b-value)
                                     new-idx (+ (:index state) jump-by)]
                                 (assoc state :index new-idx))

                               :default
                               (let [f (instr-functions instr)
                                     _ (assert f (str "No function for: " instr))
                                     new-state (f args state)
                                     new-idx (inc (:index state))]
                                 (assoc new-state :index new-idx))))))
                       start-state)))


(def real-file "twenty_five.txt")
(def quick-real-file "twenty_five_quick.txt")
(def bruce-file "bruce_25.txt")
(def file-name quick-real-file)

(def small-start 2555)
(def small-program-tags {"a" 0 "b" small-start "c" 0 :finish false})

;cpy 2 c
;jnz b 2
;jnz 1 6
;dec b
;dec c
;jnz c -4
;inc a
;jnz 1 -7
(def small-program [[:cpy 2 "c"] [:jnz "b" 2] [:fin] [:dec "b"] [:dec "c"] [:jnz "c" -4]
                    [:inc "a"] [:jnz 1 -7]])

;;
;; output of the last one before the count reached 9
;;
(defn try-start-int [instructions initial-tags]
  (fn [n]
    (:output (last (take-while #(not= (count (:output %)) 9)
                               (run-prog instructions (assoc initial-tags "a" n)))))))

(defn x []
  (let [raw-input (slurp (io/resource file-name))
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        instructions (retrieve-instructions in)
        runner (try-start-int instructions initial-tags)
        ]
    (count (take-while #(not= [0 1 0 1 0 1 0 1] %)
                       (map runner (range 0 1000))))))

(defn x-2 []
  (let [run (runner small-program)]
    (doseq [i (range 50)]
      (let [b-input (+ small-start i)
            input-tags (assoc small-program-tags "b" b-input)
            res (run input-tags)]
        (println (str b-input " ==> " res))))))

(defn x-3 []
  (doseq [i (range 50)]
    (let [b-input (+ small-start i)
          input-tags (assoc small-program-tags "b" b-input)
          res (small-program-i nil input-tags)]
      (println (str b-input " ==> " res)))))
