(ns advent-2019.day3
  (:require
    [clojure.java.io :as io]
    [clojure.set :as set]))

(defn to-int [s]
  (Long/parseLong s))

(defn abs [n]
  (if (neg? n)
    (* n -1)
    n))

(defn make-vect [s]
  (let [s (str s)
        direction (subs s 0 1)
        magnitude (to-int (subs s 1 (count s)))]
    [direction magnitude]))

(defn line->vects [line]
  (->> (read-string (str "[" line "]"))
       (map make-vect)))

(defn read-lines []
  (let [reader (io/reader (io/resource "day3.edn"))
        lines (line-seq reader)]
    (map line->vects lines)))

(let [[line-1 line-2] (read-lines)]
  (def line-1 line-1)
  (def line-2 line-2))

(defn x-2 []
  (take 3 line-1))

(defn x-3 []
  (take 10 line-2))

(def iteratees
  {"U" (fn [[x y]]
         [x (inc y)])
   "D" (fn [[x y]]
         [x (dec y)])
   "L" (fn [[x y]]
         [(dec x) y])
   "R" (fn [[x y]]
         [(inc x) y])})

(defn position+vect->positions [position [direction magnitude]]
  (->> (iterate (get iteratees direction) position)
       (take (inc magnitude))
       (drop 1)))

(defn x-4 []
  (position+vect->positions [0 0] ["U" 7]))

;;
;; We want the whole path of a line starting from [0 0]
;; We can loop-recur going thru the vectors and accumulating all-positions
;;
(defn line->positions [line]
  (loop [positions [[0 0]] line line]
    (if (seq line)
      (let [[vect & rest-of-line] line
            new-positions (position+vect->positions (last positions) vect)]
        (recur (into positions new-positions) rest-of-line))
      positions)))

(defn distance [[x1 y1] [x2 y2]]
  (+ (abs (+ x1 x2)) (abs (+ y1 y2))))

(defn x-6 []
  (distance [0 0] [3 3]))

(defn include-distance-a [origin position]
  {:position position
   :distance (distance origin position)})

(defn solve-a  [origin line-1 line-2]
  (let [line-1-path (line->positions line-1)
        line-2-path (line->positions line-2)
        crossed-wires (set/intersection (set line-1-path) (set line-2-path))
        candidates (->> (remove #{origin} crossed-wires)
                        (map (partial include-distance-a origin)))]
    (:distance (apply min-key :distance candidates))))

;; R8,U5,L5,D3 and U7,R6,D4,L4
(defn x-5 []
  (let [origin [0 0]
        line-1 [["R" 8] ["U" 5] ["L" 5] ["D" 3]]
        line-2 [["U" 7] ["R" 6] ["D" 4] ["L" 4]]]
    (solve-a origin line-1 line-2)))

(defn solve-for-a []
  (let [origin [0 0]]
    (solve-a origin line-1 line-2)))

(defn position-distance [position path]
  (->> path
       (take-while #(not= position %))
       count))

(defn x-7 []
  (position-distance [7 7] [[1 3] [4 6] [2 9] [12 3] [7 7] [9 4]]))

(defn sum-position-distance [path-1 path-2 candidate]
  (+ (position-distance candidate path-1) (position-distance candidate path-2)))

;;
;; To solve for b get the candidates as before
;; For each candidate get the position-distance for each line and sum
;; Then min-key using a different distance
;;
(defn solve-b [origin line-1 line-2]
  (let [line-1-path (line->positions line-1)
        line-2-path (line->positions line-2)
        travelled-distance-by-both-f (partial sum-position-distance line-1-path line-2-path)
        crossed-wires (set/intersection (set line-1-path) (set line-2-path))
        candidates (->> (remove #{origin} crossed-wires)
                        (map (juxt identity travelled-distance-by-both-f)))]
    (apply min-key second candidates)))

(defn solve-for-b []
  (let [origin [0 0]]
    (solve-b origin line-1 line-2)))

(comment
  (solve-for-b))
