(ns advent.fourteen
  (:require [clojure.string :as str]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn five-times? [sub-seq]
  (fn [hash]
    (let [res (str/index-of hash sub-seq)]
      res)))

(defn triple [hash]
  (first (drop-while #(< (count %) 3) (partition-by identity hash))))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn search [seed]
  (loop [mode :triple
         idx 0]
    (let [hash-val (md5 (str seed idx))
          trip (triple hash-val)]
      (case mode
        :triple (if trip
                  (recur :five-in-thousand (inc idx))
                  (recur mode (inc idx)))))))

(def seed "abc")

(defn x-1 []
  (let [c \8
        hash (md5 "abc18")
        trip (triple hash)
        to-search (apply str (repeat 5 (first trip)))
        five-times-sample "00888"
        _ (println to-search)]
    ((five-times? to-search) five-times-sample)))

(defn x []
  (search seed))
