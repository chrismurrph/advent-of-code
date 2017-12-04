(ns advent-2017.day04
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [utils :as u]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer :all]))

(def ex [["aa" "bb" "cc" "dd" "jj"]
         ["aa" "bb" "cc" "dd" "aa"]
         ["aa" "bb" "cc" "dd" "kk"]])

;; (def input (line-seq (io/reader (io/resource "day02"))))

(defn get-input []
  (->> (io/resource "2017/day04")
       slurp
       s/split-lines
       ;(take 2)
       dev/probe-off
       (mapv #(s/split % #" "))
       dev/probe-off
       ))

(defn has-dup? [xs]
  (->> (frequencies xs)
       vals
       (filter #(> % 1))
       seq))

(defn is-anagram? [x y]
  (let [perms (set (map s/join (combo/permutations x)))]
    ;(println "To test if" y "is in one of" perms "from" x)
    (boolean (perms y))))

(defn any-anagrams? [xs]
  (let [combos (u/combinations xs 2)]
    (boolean (some #(apply is-anagram? %) combos))))

(def valid-passphrase-1? (complement has-dup?))
(def valid-passphrase-2? (complement any-anagrams?))

;; ans 466
(defn x-1 []
  (let [input (get-input)]
    (->> input
         (map #(valid-passphrase-1? %))
         (filter identity)
         count)))

;; ans 251
(defn x-2 []
  (let [input (get-input)]
    (->> input
         (map #(valid-passphrase-2? %))
         (filter identity)
         count)))

(def test-input [["abcde" "fghij"]
                 ["abcde" "xyz" "ecdab"]
                 ["a" "ab" "abc" "abd" "abf" "abj"]
                 ["iiii" "oiii" "ooii" "oooi" "oooo"]
                 ["oiii" "ioii" "iioi" "iiio"]])

(deftest examples-pass
  (is (= [true false true true false]
         (mapv valid-passphrase-2? test-input))))
