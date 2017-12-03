(ns advent.day22
  (:require
    [utils :as u]
    [dev :as dev]
    [clojure.java.io :as io]
    [clojure.string :as s]
    [clojure.pprint :as pp]
    [medley.core :refer [distinct-by]]))

;;
;; In the end not essential to use breadth-first-search (but solution is very data-dependent)
;;
(defn breadth-first-search [max-steps
                            starting-grid
                            generate-possibilities
                            destination-state-fn?
                            debug?]
  (loop [already-tested #{starting-grid}
         last-round #{starting-grid}
         times 0
         most-distant-candidates []]
    (assert (set? already-tested))
    (if (< times max-steps)
      (let [_ (when debug? (println "steps done:" times "visited:" (count already-tested)))
            newly-generated (mapcat generate-possibilities last-round)
            _ (when debug? (println (str "count of new gens: " (count newly-generated))))]
        (if (seq newly-generated)
          (let [got-there? (first (filter destination-state-fn? newly-generated))]
            (if got-there?
              (do
                (when debug? (println (str "Got there with: <" got-there? ">")))
                {:steps (inc times) :res got-there?})
              (let [now-tested (into already-tested newly-generated)
                    next-round (into #{} (remove already-tested newly-generated))
                    _ (assert (seq next-round) (str "Must be in loop as removal of already-tested excludes"))
                    ]
                (recur now-tested next-round (inc times) most-distant-candidates))))
          [:dead-end "Nowhere to go:" (seq newly-generated)]))
      [:need-more-steps "Need give more steps then run again, used up:" max-steps])))

;;
;; transducer so read normal way round
;; re-seq #"\d" - puts decimals into a sequence
;; Cool b/c get co-ords, size and used in one go.
;; So first mapentry will be [0 0] [86 73]
;; This map is :data in state.
;; Notice that it goes by columns, hence need transpose to represent to humans (see view-grid)
;;
(defn parse-input [s]
  (into (sorted-map)
        (comp
          (drop 2)
          (map #(re-seq #"\d+" %))
          (map u/to-ints)
          (map #(take 4 %))
          (map #(partition 2 %))
          (map #(mapv vec %)))
        (s/split-lines s)))

(def real-data (parse-input (slurp (io/resource "2016/twenty_two.txt"))))
(def example-data (parse-input (slurp (io/resource "2016/twenty_two_example.txt"))))
(def blocking-data (parse-input (slurp (io/resource "2016/twenty_two_blocking_example.txt"))))
(def bruce-data (parse-input (slurp (io/resource "2016/twenty_two_bruce.txt"))))

(def available (partial apply -))
(def size first)
(def used second)
(def empty-node (comp zero? used))

(defn spare-space? [space-required]
  (fn [node]
    (let [avail (available node)]
      (>= avail space-required))))

(declare too-big-to-move?)

(defn viable-pair? [a b]
  (and a b
       (not (empty-node a))
       (>= (available b) (used a))))

;; transposes, because the data happens to have been read in the wrong way around, which
;; is irrelevant for all but viewing
(defn view-grid [{:keys [data]} row-count]
  (apply map vector (partition row-count (map used (vals data)))))

(defn fancy-view-grid [required {:keys [data]} row-count]
  (apply map vector (partition row-count (map (fn [node]
                                                (let [avail (available node)]
                                                  (cond
                                                    (too-big-to-move? node) \#
                                                    (>= avail required) \_
                                                    (< avail required) \.)
                                                  )) (vals data)))))

(defn pp
  ([d]
   (pp 200 d))
  ([n d]
   (binding [clojure.pprint/*print-right-margin* n]
     (clojure.pprint/pprint d))))

(defn top-right-coord [data]
  [(->> data keys (map first) (reduce max)) 0])

;;
;; max x and 0 (for y) is where :g is
;; :last-move is where :used is 0
;; There is such a point at x=20 and y=6
;; :last-move is just a coordinate, but wrapped in a vector for some reason.
;; We start off with :last-move being the empty node.
;;
(defn make-initial [data]
  {:data      data
   :g         (top-right-coord data)
   :last-move [(ffirst (filter (comp empty-node second) data))]})

(defn get-required-to-move [data]
  (let [tr (top-right-coord data)
        tr-value (get data tr)
        required-to-move (used tr-value)
        _ (assert (not= 0 required-to-move) (str "Wrong top right cooord: " tr))
        _ (println "Have to move: " required-to-move)]
    required-to-move))

(defn capable! [required-to-move]
  (fn [data]
    (let [availables (into (sorted-map) (map (fn [[k v]] [k (available v)]) data))
          capable-movers (filter (fn [[_ available]] (>= available required-to-move)) availables)
          [capable-mover & tail] capable-movers
          _ (assert (nil? tail) (str "Not just " capable-mover ", but also: " (seq capable-movers)))]
      capable-mover)))

;;
;; Ends up with same capable-mover-node as make-initial does: x20-y6
;;
(defn make-initial-2 [data]
  (let [capable-mover-node ((capable! (get-required-to-move data)) data)
        _ (assert capable-mover-node)
        _ (println "Got capable mover: " capable-mover-node)
        ]
    ;; The starting off grid state
    {:data      data
     :g         (top-right-coord data)
     :last-move [(first capable-mover-node)]
     }))

;; this is subtle: a preference for up and to the left is helpful
;; as it provides the very last disambiguation between = score moves

;; If there's a wall above then creeping to the left is the thing to do
;; Cool that can add together tuples using multiple map
(defn connections [wall-above? a]
  (let [possible-moves (if wall-above?
                         [[-1 0]]
                         [[-1 0] [0 -1] [0 1] [1 0]])]
    (map #(mapv + a %) possible-moves)))

(def testing-times 30)
(def real-times 250)
(def max-times real-times)

;;
;; g [36 0] to-pos [34 3] surrounding-pos [35 3]
;; Gives 235, but that's too big
;;
(def exclude-these-1 #{
                     [[36 0] [34 4] [35 4]]
                     [[36 0] [31 3] [32 3]]
                     [[36 0] [32 3] [33 3]]
                     })

;; Gives 243
(def exclude-these-2 #{
                     [[36 0] [35 3] [36 3]]
                     })

;; Gives 243 as well
(def exclude-these-3 #{
                       [[36 0] [35 4] [34 4]]
                       [[36 0] [35 4] [35 5]]
                       })

(def exclude-none #{})

(def exclude-these exclude-none)

(defn move-excluder [g to-pos]
  (fn [surrounding-pos]
    (let [together [g to-pos surrounding-pos]]
      (exclude-these together))))

;;
;; We are trying to get a route towards the empty node
;;
(defn possible-moves [wall-above? {:keys [last-move data g]}]
  (let [
        ;Can only call when get-required-to-move is a def
        ;_ ((capable! (-get-required-to-move data)) data)
        to-pos (first last-move)
        ;_ (println "to-pos: " to-pos)
        to-data (get data to-pos)
        excluded (move-excluder g to-pos)
        ]
    (for [from-pos (remove excluded (connections wall-above? to-pos))
          :let [from-data (get data from-pos)]
          :when (viable-pair? from-data to-data)]
      [from-pos to-pos])))

(defn make-move [{:keys [data g] :as st} [from to]]
  (let [from-node (get data from)]
    (-> st
        (update-in [:data to 1] + (used from-node))
        (assoc-in  [:data from 1] 0)
        (assoc :g  (if (= g from) to g)
               :last-move [from to]))))

(defn next-grid-states [wall-above? {:keys [last-move] :as grid-st}]
  (let [moves (->> (possible-moves wall-above? grid-st)
                   (filter #(not= last-move (reverse %))))]
    (assert (seq moves) (str "No possible moves when last move was " last-move))
    (map (partial make-move grid-st) moves)))

;;
;; distance is the sum of x and y distances. Euclidean distance
;;
(defn distance [from to]
  (if (and from to)
    (apply + (map #(Math/abs %) (map - from to)))
    (assert false (str from to))
    ))

(defn left-of [[x y]]
  [(dec x) y])

(defn above [[x y]]
  [x (dec y)])

;;
;; Adding x and y in g means top left has low value
;; We want distance between g and last-move to be small - when 0 g has made it
;;
(defn score [{:keys [g last-move]}]
  [(apply + g)
   (distance (left-of g) (first last-move))])

(defn next-level-orig [grid-state]
  (let [[first-low-scoring-grid second-low-scoring-grid] (sort-by score (next-grid-states false grid-state))
        _ (assert (not= first-low-scoring-grid second-low-scoring-grid) (str "Multiple equal scores: <" first-low-scoring-grid ">, <" second-low-scoring-grid ">"))]
    first-low-scoring-grid))

(defn too-big-to-move? [node]
  (if (nil? node)
    false
    (>= (used node) 100)))

;;
;; A wall has a lot of data that can't be shifted around. Also because it may run us into a boundary we need
;; maximum choices rather than always heading just to the left of where the :g is.
;;
(defn adjacent-to-wall? [{:keys [data g last-move]}]
  (let [[from to] last-move
        _ (assert from)
        ;from-data? (-> data (get from) too-big-to-move?)
        ;to-data? (-> data (get to) too-big-to-move?)
        above-from? (-> data (get (above from)) too-big-to-move?)
        ]
    ;(println "g, last-move, above-from?" g last-move above-from?)
    above-from?))

(defn next-level [grid-state]
  (let [a? (adjacent-to-wall? grid-state)
        sorted (sort-by score (next-grid-states a? grid-state))
        _ (assert (pos? (count sorted)))]
    (if a?
      sorted
      (dev/probe-off (take 1 sorted)))))

(def last-g (atom nil))

(defn probe-g [g]
  (when (not= @last-g g)
    (reset! last-g g)
    (println "g: " g))
  g)

;;
;; The only thing that can be done is to move the empty spot around.
;; We select firstly for G being in the top left corner, and secondly for
;; the empty space being there. We know that the empty space is going to
;; help g get there.
;;
(defn find-answer2-orig [limit data]
  (->> (iterate next-level-orig (make-initial-2 data))
       (take-while #(not= (probe-g (:g %)) [0 0]))
       (take limit)
       count))

;; => 976 was my correct answer, that this also gets
(defn x-first-part []
  (count
    (for [[pos1 data1] real-data
          [pos2 data2] real-data
          :when (not= pos1 pos2)
          :when (viable-pair? data1 data2)]
      [pos1 pos2])))

;; Gives answer of 235 now have excluded loops, but this is still too high
(defn x-2 []
  ;(find-answer2 max-times blocking-data)
  (let [data real-data
        initial-grid (make-initial-2 data)
        ;; the move only has one value in it, but that's okay I believe
        _ (println "initial-grid: " initial-grid)
        res (breadth-first-search max-times
                                  initial-grid
                                  next-level
                                  #(= (probe-g (:g %)) [0 0])
                                  false)]
    (if (map? res)
      (:steps res)
      res)))

(defn required-quantity [data]
  (let [tr (top-right-coord data)
        tr-value (get data tr)
        required-to-move (used tr-value)]
    required-to-move))

;;
;; Shows that there's only one capable mover. Works for both example and real data
;;
(defn x-3 []
  (let [data real-data
        r (required-quantity data)
        _ (println "Have to move: " r)
        availables (into (sorted-map) (map (fn [[k v]] [k (available v)]) data))
        capable-movers (filter (fn [[_ v]] (>= v r)) availables)
        ]
    capable-movers))

(defn compare-data []
  (let [real-required (required-quantity real-data)
        bruce-required (required-quantity bruce-data)]
    (pp (fancy-view-grid real-required (make-initial-2 real-data) 27))
    (pp (fancy-view-grid bruce-required (make-initial-2 bruce-data) 30))))

(defn view-data []
  (pp 20 (view-grid (make-initial-2 blocking-data) 4)))