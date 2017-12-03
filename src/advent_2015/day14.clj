(ns advent-2015.day14
  (:require [clojure.java.io :as io]
            [utils :as u]
            [dev :as dev]))

(def input
  (line-seq (io/reader (io/resource "day14"))))

(def race-duration #_1000 2503)

(defn calc-distance [race-duration {:keys [name speed run-time rest-time]}]
  (assert race-duration "nil race-duration")
  (assert speed "nil speed")
  (assert run-time "nil run-time")
  (assert rest-time "nil rest-time")
  (let [burst-time (+ run-time rest-time)
        dist-per-burst (* speed run-time)
        whole-sprints (quot race-duration burst-time)
        remaining (rem race-duration burst-time)
        remaining-dist (if (>= remaining run-time)
                         dist-per-burst
                         (* speed remaining))]
    [name (+ (* whole-sprints dist-per-burst) remaining-dist)]))

;;Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.
;;speed duration rest-time
(defn get-data [in]
  (->> in
       ;(take 8)
       dev/probe-off
       (map (partial re-matches #"(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds."))
       (map next)
       (map (juxt first (fn [[a b c d]]
                          {:name a :speed (u/string->int b) :run-time (u/string->int c) :rest-time (u/string->int d)})))
       (into {})))

(defn part-1 []
  (let [m (get-data input)
        all-names (-> m keys distinct)
        calculator (partial calc-distance race-duration)]
    (->> all-names
         (map (comp second calculator m))
         (apply max))))

;; Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
;; 1120 km
(defn x-2 []
  (calc-distance 1000 {:speed 14 :run-time 10 :rest-time 127}))

;;
;; Has them all and for any point in time gives you the current winners are.
;;
(defn points-calculator [m]
  (let [all-names (-> m keys distinct)]
    (fn [n]
      (let [calculator (partial calc-distance n)]
        (->> all-names
             (map (comp calculator m))
             (reduce (fn [[greatest-names greatest-dist] [name dist]]
                       (assert (vector? greatest-names))
                       (cond
                         (empty? greatest-names) [[name] dist]
                         (> dist greatest-dist) [[name] dist]
                         (= dist greatest-dist) [(conj greatest-names name) greatest-dist]
                         :default [greatest-names greatest-dist]
                         )
                       )
                     [[] 0]))))))

(defn part-2 []
  (let [m (get-data input)
        calc-all-fn (points-calculator m)]
    (->> (range 1 (inc race-duration))
         (map calc-all-fn)
         (mapcat first)
         frequencies
         (map second)
         (apply max))))

