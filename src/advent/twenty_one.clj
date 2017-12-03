(ns advent.twenty-one
  (:require [instaparse.core :as insta]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [utils :as u]
            [clojure.java.io :as io])
  (:import (java.io StringReader BufferedReader)))

(def steps ["move position 0 to position 3"
            "rotate right 0 steps"
            "rotate right 1 step"
            "move position 1 to position 5"
            "swap letter h with letter b"
            "swap position 6 with position 3"
            "reverse positions 1 through 3"
            "rotate based on position of letter c"
            ])

(def grammar-1
  (insta/parser
    "<S> = cmd
     move = <'move position '> int <' to position '> int
     direction = 'right' | 'left'
     rotate-direction = <'rotate '> direction <' '> int (<' steps'> | <' step'>)
     rotate-based = <'rotate based on position of letter '> letter
     letter = #'\\w+'
     swap-letter = <'swap letter '> letter <' with letter '> letter
     swap-position = <'swap position '> int <' with position '> int
     reverse = <'reverse positions '> int <' through '> int
     cmd = move | rotate-direction | rotate-based | swap-letter | swap-position | reverse
     int = #'[-0-9]+'
     "
    ))

(defn left-rotate [s n]
  (let [rem-n (rem n (count s))]
    (str (subs s rem-n) (subs s 0 rem-n))))

(defn right-rotate [s n]
  (let [size (count s)
        rem-n (rem n size)
        o (- size rem-n)
        len size]
    (str (subs s o len) (subs s 0 o))))

;; rotate based on position of letter d finds the index of letter d (4), then rotates the string right once, plus a number of times
;; equal to that index, plus an additional time because the index was at least 4, for a total of 6 right rotations: decab.
(defn rotate-position [s a]
  (let [x (s/index-of s a)
        extra (if (>= x 4) 1 0)
        times (+ 1 x extra)
        ;_ (println times s)
        ]
    (right-rotate s times)))

(defn reverse-span [s m n]
  (let [sub (subs s m (inc n))
        before (subs s 0 m)
        after (subs s (inc n) (count s))
        rev (s/reverse sub)]
    ;[m n s before rev after]
    (str before rev after)
    ))

(defn insert [s idx letter]
  (let [before (subs s 0 idx)
        after (subs s idx (count s))]
    (str before letter after)))

(defn move [s m n]
  (let [to-move (get s m)
        before (subs s 0 m)
        after (subs s (inc m) (count s))
        now-removed (str before after)
        res (insert now-removed n to-move)]
    ;[now-removed to-move n res]
    res))

(defn -swap-letter-wrong [s b a]
  (let [
        -x (s/index-of s a)
        -y (s/index-of s b)
        x-use (min -x -y)
        y-use (max -x -y)
        _ (assert (> y-use x-use))
        ;_ (println "swapping letter" a "at" x-use)
        ;_ (println "with letter" b "at" y-use "in" s)
        before (subs s 0 x-use)
        _ (assert (not (neg? x-use)))
        _ (assert (not (neg? y-use)))
        _ (assert (string? s))
        middle (subs s (inc x-use) y-use)
        after (subs s (inc y-use) (count s))
        res (str before b middle a after)]
    res))

(defn fixed-swap-letter [s y x]
  (let [use-x (first x)
        use-y (first y)]
    (apply str (replace {use-x use-y use-y use-x} s))))

(defn my-swap-position [s -y -x]
  (let [
        x-use (min -x -y)
        y-use (max -x -y)
        letter-at-x (nth s x-use)
        letter-at-y (nth s y-use)
        before (subs s 0 x-use)
        middle (subs s (inc x-use) y-use)
        after (subs s (inc y-use) (count s))
        res (str before letter-at-y middle letter-at-x after)]
    res))

;; Bruce's operations
(defn bruce-swap-letter [s x y]   (replace {x y y x} s))

(defn bruce-swap-position [s x y] (bruce-swap-letter s (nth s x) (nth s y)))

(defn bruce-rotate-left [s n]     (take (count s) (drop n (cycle s))))

(defn bruce-rotate-right [s n]    (reverse (bruce-rotate-left (reverse s) n)))

(defn bruce-move-position [s x y]
  (let [l   (nth s x)
        res (remove #(= l %) s)]
    (concat (take y res) (cons l (drop y res)))))

(defn bruce-reverse-positions [s x y]
  (let [part-size (inc (- y x))]
    (concat (take x s)
            (reverse (take part-size (drop x s)))
            (drop (+ x part-size) s))))

(defn bruce-rotate-based [s l]
  (let [i (.indexOf s l)]
    (bruce-rotate-right s (+ (inc i) (if (>= i 4) 1 0)))))

(def rotate-based #_rotate-position bruce-rotate-based #_(fn [_ _] (assert false)))
(def reverse-positions #_reverse-span bruce-reverse-positions)
(def move-position #_move bruce-move-position)
(def rotate-right #_right-rotate bruce-rotate-right #_(fn [_ _] (assert false)))
(def rotate-left #_left-rotate bruce-rotate-left)
(def swap-position my-swap-position #_bruce-swap-position #_(fn [_ _ _] (assert false)))
(def swap-letter fixed-swap-letter #_bruce-swap-letter #_(fn [_ _ _] (assert false)))

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

;; Mine are string based, so needed this when starting to use Bruce's functions
(def trans (partial apply (comp #_dev/probe-on str)))

(defn execute-cmd-scramble [cmd acc arg-1 arg-2]
  (case cmd
    :move (move-position acc arg-1 arg-2)
    :rotate-based (rotate-based acc arg-1)
    :rotate-direction (cond
                        (= :right arg-1)
                        (rotate-right acc arg-2)
                        (= :left arg-1)
                        (rotate-left acc arg-2)
                        )
    :swap-letter (swap-letter acc arg-1 arg-2)
    :swap-position (swap-position acc arg-1 arg-2)
    :reverse (reverse-positions acc arg-1 arg-2)))

;; not invertable so search (from Bruce)
(defn inv-rotate-based [s l]
  (->> (map #(rotate-left s %) (range (count s)))
       (filter #(= (rotate-based % l) s))
       first))

;;
;; This not finished yet. Skipped ahead to the next day. Will come back to this in Ordinary Time.
;;
(defn execute-cmd-unscramble [cmd acc arg-1 arg-2]
  (case cmd
    :move (move-position (s/reverse acc) arg-1 arg-2)
    :rotate-based (inv-rotate-based acc arg-1)
    :rotate-direction (cond
                        (= :right arg-1)
                        (rotate-left acc arg-2)
                        (= :left arg-1)
                        (rotate-right acc arg-2)
                        )
    :swap-letter (swap-letter acc arg-1 arg-2)
    :swap-position (swap-position (s/reverse acc) arg-1 arg-2)
    :reverse (reverse-positions acc arg-1 arg-2)))

(defn run-instructions [input instructions executor]
  (let [res (reduce
              (fn [acc ele]
                (let [
                      ;_ (println ele)
                      [cmd & [arg-1 arg-2]] ele
                      cmd-res (executor cmd acc arg-1 arg-2)]
                  (trans cmd-res)))
              input
              instructions)]
    (apply str res)))

;;
;; I replaced my instructions with Bruce's, thinking I must have got one wrong. Result from running
;; Bruce's instructions is:
;; "dhcegafb"
;; Result from mine is:
;; "fdgeabhc"
;;
;; From Bruce's notes this is the right answer:
;; "gbhafcde", but that's CRAP
;; ---------------> "agcebfdh" GETS STAR, running his code
;; So more than the instructions are wrong!
;; Unsurprisingly my commands with Bruce's instructions produces another (wrong) result again:
;; "bghcedfa"
;; My function that was in error was -swap-letter, so have fixed that.
;; I've checked order of instructions, that all are called, and swapped params around to see if the right
;; answer can come out, all to no avail. Also checked that every instruction is in the file.
;; Next I should clone his repo and compare all intermediate results, to see where they differ.
;; Bruce 4th, 5th: (h c d a e b f g) [b c d a e h f g]
;; Me 4th, 5th: hcdaebfg hcdaebfg
;; So my parser is somehow ignoring instruction that swaps first and sixth letters, actually:
;; "swap letter h with letter b"
;; swap-letter is the one I got wrong, so maybe new untested version is doing nothing x-5 to test.
;; Indeed problem is it needs to be a letter coming in, so using spec would have caught this error.
;;
(defn x []
  (let [
        raw-input (slurp (io/resource "2016/twenty_one.txt"))
        ;raw-input steps
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        instructions (retrieve-instructions in)]
    ;(pp/pprint instructions)
    (run-instructions "abcdefgh" instructions execute-cmd-scramble)
    ))

;; "afhdbegc" is correct answer
(defn part-2 []
  (let [
        raw-input (slurp (io/resource "2016/twenty_one.txt"))
        ;raw-input steps
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        instructions (retrieve-instructions in)]
    ;(pp/pprint instructions)
    (run-instructions "fbgdceah" (reverse instructions) execute-cmd-unscramble)
    ))

(defn x-2 []
  (swap-letter "holiday" "o" "i"))

(defn x-3 []
  (rotate-position "ecabd" "d"))

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
        position-swapped (swap-position input 4 0)
        letter-swapped (swap-letter position-swapped "d" "b")
        reversed (reverse-span letter-swapped 0 4)
        rotated (left-rotate reversed 1)
        moved-1 (move rotated 1 4)
        moved-2 (move moved-1 3 0)
        rotated-1 (rotate-position moved-2 "b")
        rotated-2 (rotate-position rotated-1 "d")
        _ (println rotated-2)
        ]))

;; Bruce 4th, 5th: (h c d a e b f g) [b c d a e h f g]
;; "swap letter h with letter b"
(defn x-5 []
  (fixed-swap-letter "hcdaebfg" "h" "b"))
