(ns advent.twenty-one
  (:require [instaparse.core :as insta]
            [clojure.string :as str])
  (:import (java.io StringReader BufferedReader)))

(def steps ["move position 0 to position 3"
            "rotate right 0 steps"
            "rotate right 1 step"
            "move position 1 to position 5"
            "swap letter h with letter b"
            "swap position 6 with position 3"
            "reverse positions 1 through 3"
            ])

(def grammar-1
  (insta/parser
    "<S> = cmd
     move = <'move position '> int <' to position '> int
     direction = 'right' | 'left'
     rotate = <'rotate '> direction <' '> int (<' steps'> | <' step'>)
     letter = #'\\w+'
     swap-letter = <'swap letter '> letter <' with letter '> letter
     swap-position = <'swap position '> int <' with position '> int
     reverse = <'reverse positions '> int <' through '> int
     cmd = move | rotate | swap-letter | swap-position | reverse
     int = #'[-0-9]+'
     "
    ))

(defn swap-position [x y]
  (fn [s]
    (let [letter-at-x (nth s x)
          letter-at-y (nth s y)
          before (subs s 0 x)
          middle (subs s (inc x) y)
          after (subs s (inc y) (count s))
          res (str before letter-at-y middle letter-at-x after)]
      res)))

(defn left-rotate [n]
  (fn [s]
    (str (subs s n) (subs s 0 n))))

(defn right-rotate [n]
  (fn [s]
    (let [o (- (count s) n)
          len (count s)]
      (str (subs s o len) (subs s 0 o)))))

(defn rotate-position [a]
  (fn [s]
    (let [x (str/index-of s a)
          extra (if (>= x 4) 1 0)
          times (+ 1 x extra)]
      ((right-rotate times) s))))

(defn reverse-span [m n]
  (fn [s]
    (let [sub (subs s m (inc n))
          before (subs s 0 m)
          after (subs s (inc n) (count s))
          rev (str/reverse sub)]
      ;[m n s before rev after]
      (str before rev after)
      )))

(defn insert [s idx letter]
  (let [before (subs s 0 idx)
        after (subs s idx (count s))]
    (str before letter after)))

(defn move [m n]
  (fn [s]
    (let [to-move (get s m)
          before (subs s 0 m)
          after (subs s (inc m) (count s))
          now-removed (str before after)
          res (insert now-removed n to-move)]
      ;[now-removed to-move n res]
      res)))

(defn swap-letter-hof [a b]
  (fn [s]
    (let [
          x (str/index-of s a)
          y (str/index-of s b)
          before (subs s 0 x)
          middle (subs s (inc x) y)
          after (subs s (inc y) (count s))
          res (str before b middle a after)]
      res)))

(defn to-hiccup [s]
  (->> (first (grammar-1 s))
       (insta/transform
         {:int clojure.edn/read-string
          :direction keyword
          :letter str
          })))

(defn retrieve-instructions [steps]
  (let [
        ;_ (println steps)
        lines steps
        instructions (map to-hiccup steps)
        ;_ (println instructions)
        ]
    (mapcat identity (map next instructions))))

(defn x []
  (let [
        ;raw-input (slurp "./advent/twenty_one.txt")
        ;raw-input steps
        ;in (line-seq (BufferedReader. (StringReader. raw-input)))
        instructions (retrieve-instructions steps)]
    instructions))

(defn x-2 []
  ((swap-letter-hof "o" "i") "holiday"))

(defn x-3 []
  ((move 0 2) "abcd"))

;swap position 4 with position 0 swaps the first and last letters, producing the input for the next step, ebcda.
;swap letter d with letter b swaps the positions of d and b: edcba.
;reverse positions 0 through 4 causes the entire string to be reversed, producing abcde.
;rotate left 1 step shifts all letters left one position, causing the first letter to wrap to the end of the string: bcdea.
;move position 1 to position 4 removes the letter at position 1 (c), then inserts it at position 4 (the end of the string): bdeac.
;move position 3 to position 0 removes the letter at position 3 (a), then inserts it at position 0 (the front of the string): abdec.
;rotate based on position of letter b finds the index of letter b (1), then rotates the string right once plus a number of times equal to
; that index (2): ecabd.
;rotate based on position of letter d finds the index of letter d (4), then rotates the string right once, plus a number of times
; equal to that index, plus an additional time because the index was at least 4, for a total of 6 right rotations: decab.
(defn x-4 []
  (let [input "abcde"
        ]))
