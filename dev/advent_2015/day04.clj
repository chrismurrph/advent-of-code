(ns advent-2015.day04)

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn mine [code zeros-len]
  (let [match-with (apply str (take zeros-len (repeat "0")))]
    (->> (range)
         (map (juxt #(str code %) identity))
         (map (juxt (comp md5 first) second))
         (filter #(= (apply str (take zeros-len (first %))) match-with))
         first)))

(defn part-1 []
  (mine "ckczppom" 5))

(defn part-2 []
  (mine "ckczppom" 6))
