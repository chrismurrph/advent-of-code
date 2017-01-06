(ns advent.five
  (:require [clojure.string :as s]
            [utils :as u]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

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

(defn x []
  (md5 "abc3231929"))

(defn x-1 []
  (five-leading 0 "abc"))

(defn x-2 []
  (map (fn [hash] (take 2 (drop 5 hash))) (:res (get-password "uqwqemis")))
  )

