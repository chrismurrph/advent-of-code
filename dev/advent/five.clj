(ns advent.five
  (:require [clojure.string :as s]
            [utils :as u]
            [medley.core :refer [distinct-by]]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

;;
;; For the 2nd part what's at 6th position has to be between 0 and 7 inclusive to be a number
;; that provides the position.
;;
(defn right-hash? [s]
  (fn [num]
    (let [st (str s num)
          hashed (md5 st)
          position (u/string->int-not-strict (str (nth hashed 5)))]
      (when (and ((fnil >= 10) position 0) ((fnil <= 10) position 7) (s/starts-with? hashed "00000"))
        {:fine-hash hashed :num num :position position :character (nth hashed 6)}))))

(defn five-leading [start s]
  (let [numbers (iterate inc start)
        hash-f (right-hash? s)
        res (some hash-f numbers)]
    res))

(defn get-password [s]
  (reduce
    (fn [{:keys [at res] :as acc} _]
      (let [{:keys [fine-hash num]} (five-leading at s)]
        {:at (inc num) :res (conj res fine-hash)}))
    {:at 0 :res []}
    (range 13)))

(defn -x []
  (md5 "abc3231929"))

;;
;; No longer gives correct answer for part 1
;; (Would need an older version of right-hash?)
;;
(defn -x-1 []
  (five-leading 0 "abc"))

(def my-input "uqwqemis")

(declare find-password)

;;
;; Doesn't seem to work either
;;
(defn -x-2 []
  (map (fn [hash] (take 2 (drop 5 hash))) (:res (find-password my-input))))

;;
;; Rest from Bruce
;;

(def bruce-input "ffykfhsq")

(defn find-password [code]
  (transduce
    (comp
      (map #(md5 (str code %)))
      (filter #(= "00000" (subs % 0 5)))
      (map #(nth % 5))
      (take 8))
    conj
    []
    (range)))

;;
;; Is the non-transducer version just as fast?
;; Slightly slower
;;
(defn find-pw [code]
  (->> (range)
       (map #(md5 (str code %)))
       (filter #(= "00000" (subs % 0 5)))
       (map #(nth % 5))
       (take 8)))

#_ (def res (time (find-password input)))
;; => [\c \6 \6 \9 \7 \b \5 \5]
;; 23.4 seconds baby!
;; For me much longer: "Elapsed time: 80974.586602 msecs"
;; Non transducer version is fine:
;;                     "Elapsed time: 86720.013196 msecs"
;; => (\1 \a \3 \0 \9 \9 \a \a)
(defn x-1 []
  (time (doall (find-pw my-input))))

(defn find-codes-2 [code]
  (transduce
    (comp
      (map #(md5 (str code %)))
      ;; all zeros then a number from 0->7
      (filter #(and (= "00000" (subs % 0 5))
                    (> 8 (Integer/parseInt (subs % 5 6) 16))))
      ;; make a vector of the position and the character
      (map (juxt #(Integer/parseInt (str (nth % 5)))
                 #(nth % 6)))
      ;; we don't want repeating positions
      (distinct-by first)
      ;; we need to fill up positions 0 to 7
      (take 8))
    conj
    []
    (range)))

;;
;; Each code is [position character].
;; accumulator starts of being ________, and we put character into position.
;;
(defn result-password [codes]
  (reduce #(assoc %1 (first %2) (second %2)) (vec (replicate 8 '_)) codes))

#_ (def res2 (time (result-password (find-codes-2 input))))
;; => [\8 \c \3 \5 \d \1 \a \b]
;; 110 seconds

(defn x-2 []
  (time (result-password (find-codes-2 my-input))))

