(ns hackerrank.palindromic)

(defn palindromes-count [s]
  )

(defn comb [k l]
  (if (= 1 k) (map vector l)
              (apply concat
                     (map-indexed
                       #(map (fn [x] (conj x %2))
                             (comb (dec k) (drop (inc %1) l)))
                       l))))

(defn all-subsets [s]
  (apply concat
         (for [x (range 1 (inc (count s)))]
           (map #(into #{} %) (comb x s)))))

(defn- power-set [word]
  (let [add-char-seqs #(clojure.set/union %1 (map (partial str %2) %1))]
    (reduce add-char-seqs #{""} word)))

