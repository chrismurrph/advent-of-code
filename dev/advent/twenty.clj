(ns advent.twenty
  (:require [clojure.string :as str]
            [utils :as u])
  (:import (java.io StringReader BufferedReader)))

(defn make-obj [input]
  (let [[lower upper] (str/split input #"-")]
    {:lower (u/string->int lower) :upper (u/string->int upper)}))

(defn lower-abutting [int1 int2]
  (let [diff (- int2 int1)]
    (>= diff -1)))

(defn upper-abutting [int1 int2]
  (let [diff (- int1 int2)]
    (>= diff -1)))

(defn intersecting-ranges? [range1 range2]
  (assert (= (keys range1) [:lower :upper]) range1)
  (assert (= (keys range2) [:lower :upper]) range2)
  (when
    (or (and (lower-abutting (:lower range1) (:lower range2)) (upper-abutting (:upper range1) (:lower range2)))
        (and (lower-abutting (:lower range1) (:upper range2)) (upper-abutting (:upper range1) (:upper range2))))
    {:lower (min (:lower range1) (:lower range2))
     :upper (max (:upper range1) (:upper range2))}))

(defn merge-into-rest [ips]
  (let [[head & tail] ips]
    (reduce
      (fn [{:keys [res left-overs]} ele]
        (let [new-res (intersecting-ranges? res ele)]
          (if new-res
            {:res new-res :left-overs left-overs}
            {:res res :left-overs (conj left-overs ele)})))
      {:res head :left-overs []}
      tail)))

(defn big-merge [ips]
  (loop [ips ips
         results []]
    (let [{:keys [res left-overs]} (merge-into-rest ips)]
      (if (empty? left-overs)
        (conj results res)
        (recur left-overs (conj results res))))))

(defn new-ips-count [lower-range upper-range]
  (dec (- (:lower upper-range) (:upper lower-range))))

;;
;; Maximum IP address possible
;;
(def max-ip 4294967295)

(defn x []
  (let [input (slurp "./advent/twenty.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        series (map make-obj raw-series)
        sorted (sort-by :lower series)
        ;;examples (take 20 sorted)
        ;_ (println examples)
        ;in-tuples (partition 2 examples)
        ;_ (println in-tuples)
        ;res (map #(apply intersecting-ranges? %) in-tuples)
        ip-ranges (sort-by :lower (big-merge sorted))
        first-ip (first ip-ranges)
        final-ip (last ip-ranges)
        _ (assert (= (:upper final-ip) max-ip))
        _ (println "first:" first-ip "last:" final-ip)
        _ (println "last 2:" (drop (- (count ip-ranges) 2) (sort-by :upper (big-merge sorted))))
        parted (partition 2 1 ip-ranges)
        res (reduce
              (fn [acc ele]
                (let [[left right] ele]
                  (+ acc (new-ips-count left right))))
              0
              parted)
        ]
    res))

(defn x-1 []
  (new-ips-count {:lower 0, :upper 31053879} {:lower 31053881, :upper 50881439}))

(defn x-2 []
  (let [input (slurp "./advent/twenty.txt")
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        series (map make-obj raw-series)
        uppers (map :upper series)]
    (apply max uppers)))
