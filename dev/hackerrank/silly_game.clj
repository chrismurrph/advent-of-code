(ns hackerrank.silly-game)

#_(defn any? [l]
  (reduce #(or %1 %2) l))

(def certainty 5)

(defn prime-1? [n]
  (.isProbablePrime (BigInteger/valueOf n) certainty))

;; has 1 being prime
#_(defn prime-2? [n]
  (empty? (filter #(= 0 (mod n  %)) (range 2 n))))

(defn prime-3? [n]
  (or (= 2 n)
      (not-any? #(= 0 (mod n %)) (cons 2 (range 3 (inc (Math/sqrt n)) 2)))))

(defn gen-primes []
  (letfn [(sieve [s]
            (cons (first s)
                  (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                           (rest s))))))]
    (sieve (iterate inc 2))))

(defn x []
  (take 10 (gen-primes)))

(defn prime-4? [n]
  (filter #(= n %) (gen-primes)))

(def input ["3"
            "1"
            "2"
            "5"])

(def prime? prime-1?)

(defn lowest-prime [ns]
  (first (filter prime? ns)))

(defn game-turn [nums]
  (let [lowest (lowest-prime nums)]
    (when lowest
      (let [multiples (into #{} (filter (fn [num] (zero? (rem num lowest))) nums))
            new-set (remove multiples nums)]
        new-set))))

(defn other-player [in]
  (if (= in "Bob")
    "Alice"
    "Bob"))

;; Gives winner
(defn game [n]
  (let [nums (range 1 (inc n))]
    (loop [nums nums
           player "Alice"]
      (let [res (game-turn nums)]
        (if res
          (recur res (other-player player))
          (other-player player))))))

(defn play []
  (let [
        ;input (line-seq (java.io.BufferedReader. *in*))
        numbers (map #(Integer/parseInt %) (next input))
        winners (map game numbers)]
    (doseq [winner winners]
      (println winner))))


