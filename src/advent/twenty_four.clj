(ns advent.twenty-four
  (:require [utils :as u]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def sample-layout '("###########"
                      "#0.1.....2#"
                      "#.#######.#"
                      "#4.......3#"
                      "###########"))

(def actual-layout (line-seq (io/reader (io/resource "twenty_four.txt"))))

;; Works but we won't need it
#_(defn coords-of [layout num]
  (assert (number? num))
  (let [ch (str num)
        width (-> layout first count)
        idx (-> (apply str layout)
                (s/index-of ch))]
    [(rem idx width) (int (/ idx width))]))

(defn whats-at [layout]
  (fn [[x y :as coord]]
    [coord (nth (nth layout y) x)]))

(defn ch->int [ch]
  (u/string->int? (str ch)))

(defn all-numeric-coords [layout]
  (let [width (-> layout first count)
        as-str (apply str layout)
        indexes (u/indexes-by (comp u/string->int? str) as-str)
        ]
    (->> indexes
         (map
           (juxt #(ch->int (nth as-str %)) (juxt #(rem % width) #(int (/ % width))))
           #_(fn [idx] [(ch->int (nth as-str idx)) [(rem idx width) (int (/ idx width))]])
              )
         (sort-by first))))

(defn x-4 []
  (all-numeric-coords sample-layout))

;; up, down, left, right
(def moves [[0 -1]
            [0 1]
            [-1 0]
            [1 0]])
;;
;; Given [1 1] need to return [[2 1] [1 2]]
;;
(defn gen-moves [layout]
  (fn [[coord-x coord-y]]
    (let [possibilities (mapv (fn [[x y]] [(+ x coord-x) (+ y coord-y)]) moves)
          coord-values (map (whats-at layout) possibilities)
          safe-moves (filter (fn [[coord ch]] (or (= \. ch) (ch->int ch))) coord-values)
          res (map first safe-moves)]
      res)))

(defn x-6 []
  ((gen-moves sample-layout) [1 1]))

(defn dist-between [layout]
  (fn [[coord-1 coord-2]]
    (u/breath-first-search coord-1 #((gen-moves layout) %) (fn [curr] (= curr coord-2)))))

;;
;; Lets work out the minimum number of steps between 1 and 2
;; [1 [3 1]] [2 [9 1]]
;; ongoing state can be the coord are at
;;
(defn x-5 []
  (let [starting-lab [3 1]]
    (u/breath-first-search starting-lab #((gen-moves sample-layout) %) (fn [curr] (= curr [9 1])))))

(defn pair-provider [locations]
  (fn [[from-num to-num]]
    (let [pair [(get locations from-num) (get locations to-num)]]
      pair)))

(defn create-distances [layout coords]
  (let [locations (into {} coords)
        combos (u/combinations (keys locations) 2)
        provider (pair-provider locations)
        location-pairs (map provider combos)
        distances (map (dist-between layout) location-pairs)
        res (into {} (map (fn [combo dist] [(set combo) dist]) combos distances))]
    res))

;;
;; Helps to know all the combinations we will do distances for
;;
(defn x-1 []
  (u/combinations [0 1 2 3 4] 2))

(def sample-distances
  {#{0 1} 2
   #{0 2} 8
   #{0 3} 10
   #{0 4} 2
   #{1 2} 6
   #{1 3} 8
   #{1 4} 4
   #{2 3} 2
   #{2 4} 10
   #{3 4} 8})

(defn distance-finder [distances]
  (fn [from-to-set]
    (assert (set? from-to-set))
    (let [res (distances from-to-set)
          _ (assert res (str "bad lookup for" from-to-set))]
      res)))

(defn distance-hof [distances]
  (assert distances)
  ;(println "using distances" distances)
  (fn [path]
    (assert path)
    ;(println "using path" path)
    (let [partitioned-path (remove (fn [[from to]] (= from to)) (partition 2 1 path))
          ;_ (println "path" partitioned-path)
          ]
      [(->> partitioned-path
            (map set)
            (map (distance-finder distances))
            (reduce +)) path])))

(defn least-steps [return-back? distances path]
  (let [filter-fn (if return-back? #(= 0 (last %)) (constantly true))]
    (->> (combo/permutations path)
         (map #(conj % 0))
         ; Should solve 2nd
         (filter filter-fn)
         ;(map dev/probe-on)
         (map (distance-hof distances))
         (sort-by first)
         first
         ;second
         )))

(defn x-2 []
  (least-steps false sample-distances [1 2 3 4]))

(defn answer []
  (let [solve-part-2? true
        layout actual-layout
        coords (all-numeric-coords layout)
        distances-map (create-distances layout coords)
        ;; putting 0 on end to answer second part
        ;; path (conj (into [] (remove #(= 0 %) (map first coords))) 0)
        ordered=path (map first coords)
        _ (println distances-map)
        _ (println coords)
        _ (println ordered=path)
        ]
    (least-steps solve-part-2? distances-map ordered=path)))

(defn x-8 []
  (println (nth (nth actual-layout 10) 10))
  (println ((whats-at actual-layout) [27 28])))
