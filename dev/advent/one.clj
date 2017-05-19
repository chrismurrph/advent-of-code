(ns advent.one
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [utils :as u]))

(defn new-dir [dirn turn]
  (cond
    (and (= dirn :north) (= turn :right)) :east
    (and (= dirn :north) (= turn :left)) :west
    (and (= dirn :east) (= turn :left)) :north
    (and (= dirn :east) (= turn :right)) :south
    (and (= dirn :south) (= turn :right)) :west
    (and (= dirn :west) (= turn :left)) :south
    (and (= dirn :south) (= turn :left)) :east
    (and (= dirn :west) (= turn :right)) :north
    :default (assert false (str "Need for " dirn " and " turn))
    ))

;;
;; co-ords are in [x y] form
;;
(defn new-place [coord dir dist]
  (let [_ (assert dir)
        add-fn (case dir
                 :north (fn [[x y]] (vector x (inc y)))
                 :south (fn [[x y]] (vector x (dec y)))
                 :east (fn [[x y]] (vector (inc x) y))
                 :west (fn [[x y]] (vector (dec x) y)))
        ;res (repeatedly dist #(add-fn coord))
        res (reduce (fn [acc ele]
                      (add-fn acc))
                    coord
                    (range dist))
        ]
    res))

(defn vert-fn [x]
  (fn [y]
    [x y]))

(defn horiz-fn [y]
  (fn [x]
    [x y]))

(defn gen-points [[x1 y1] [x2 y2]]
  (let [vert-line? (= x1 x2)
        horiz-line? (= y1 y2)
        _ (assert (or vert-line? horiz-line?))
        least (if vert-line? (min y1 y2) (min x1 x2))
        greatest (if vert-line? (max y1 y2) (max x1 x2))
        f (if vert-line? (vert-fn x1) (horiz-fn y1))
        points (mapv f (range least (inc greatest)))
        ]
    points))

(def right (first "R"))

(defn answer [instructions]
  (let [res (reduce (fn [{:keys [locn dirn points-travelled done?]} ele]
                      (if done?
                        {:locn locn :dirn dirn :points-travelled points-travelled :done? done?}
                        (let [[dir-in & count-str] ele
                              dist (u/string->int (apply str count-str))
                              ;_ (println "dist " dist)
                              turn (if (= dir-in right) :right :left)
                              new-dirn (new-dir dirn turn)
                              moved-to (new-place locn new-dirn dist)
                              new-points (remove #{moved-to} (gen-points locn moved-to))
                              ;_ (println (str "new points: " new-points ", already: " (first points-travelled)))
                              revisit? (some (set new-points) points-travelled)
                              ]
                          (if revisit?
                            (let [up-to-points-travelled nil]
                              {:locn revisit? :dirn new-dirn :points-travelled up-to-points-travelled :done? true})
                            (let [new-points-travelled (concat points-travelled new-points)]
                              {:locn moved-to :dirn new-dirn :points-travelled new-points-travelled :done? false}))
                          )))
                    {:locn [0 0] :dirn :north :points-travelled [] :done? false}
                    instructions)]
    (:locn res)))

;;
;; This is part 2, gives 113
;; Part 1 was overwritten
;;
(defn x-1 []
  (let [input (slurp (io/resource "one.txt"))
        instructions (s/split input #", ")
        [x y] (answer instructions)
        blocks-away (+ (u/abs x) (u/abs y))]
    blocks-away))

(defn x-2 []
  (let [[x y] [151 -100]]
    (+ (u/abs x) (u/abs y))))

(defn x-3 []
  (gen-points [1 1] [5 1]))

;;
;; Following is copy of Bruce's solution, with my comments
;;

;;
;; Reading it in as a vector gets rid of the commas. But then each element becomes a symbol. Hence (map name) to
;; convert each of them to strings:
;; R2, L1, R2, R1, R1, L3, R3, L5,
;;
(def data (->> (str "[" (slurp (io/resource "one.txt")) "]")
               read-string
               (map name)))

;;
;; Each coming into mapping function is a string, such as "R21".
;; After juxt each will be like [\R 21]
;;
(defn parse-data [d]
  (map (juxt first #(Integer/parseInt (apply str (rest %)))) d))

;;
;; Shows that juxt not required - just gets rid of need for an argument
;;
(defn parse-data [d]
  (map (fn [x] [(first x) (Integer/parseInt (apply str (rest x)))]) d))

;;
;; Cumulative effect of all the lefts and rights can give us directions when realise that turning R 4 times is the
;; same as not having done any turning. Bruce is using cartesian coords here, as [0 1] is a move north, that corresponds
;; to 1 or 5 or 9. Directions are then repeated so R2 becomes [1 0] [1 0]. Interesting to see that `mapcat` works with
;; multiple colls. repeat takes 1 and returns a list so is ideal for mapcat where the lists need to be flattened.
;; Last reductions is cumulatively adding the xs and ys to give us actual positions.
;; Example:
;; ((partial map +) '(1 2) '(3 4))
;; (map + '(1 2) '(3 4))
;; => (4 6)
;; reductions takes a function and `(partial map +)` is the function version of `map +`
;;
(defn positions [d]
  (->> (map first d)
       (reductions #(({\L dec \R inc} %2) %1) 0)
       rest
       (map #(mod % 4))
       (map [[0 1] [1 0] [0 -1] [-1 0]])
       (mapcat repeat (map second d))
       (reductions (partial map +) (list 0 0))
       ))

;;
;; Adding absolute x to absolute y
;;
(defn point-to-dist [p]
  (->> p
       (map #(Math/abs %))
       (reduce +)))

(defn bruce-part-1 []
  (->> data
       parse-data
       positions
       last
       point-to-dist
       ))

;;
;; Looking for position where you cross your own path for the first time.
;; We create every possible path as move outwards, then look for first one that crosses over itself.
;; Luckily reductions returns a lazy seq. We are conjing onto a list, meaning we get end up with the
;; path back to [0 0] each iteration, obviously getting longer with each iteration.
;; Because we get the path back we can see if the last position is in the set of all prior postions.
;; (Obviously last position is in first place in the list)
;;
(defn bruce-part-2 []
  (->> data
       parse-data
       positions
       (reductions conj (list))
       (filter (fn [[x & xs]] ((set xs) x)))
       ffirst
       point-to-dist))

(defn x []
  (->> data
       parse-data
       positions
       (reductions conj (list))))