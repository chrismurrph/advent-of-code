(ns advent.twenty-two
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [utils :as u]
            [clojure.string :as str])
  (:import (java.io StringReader BufferedReader)))

(defn all-but-last [in]
  (let [size (count in)]
    (apply str (take (dec size) in))))

(defn number-char? [x]
  (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} x))

(defn number-after [s a]
  (let [x-idx (str/index-of s a)]
    (u/string->int (apply str (seq (take-while #(number-char? %) (drop (inc x-idx) s)))))))

(defn get-x [grid-id]
  (number-after grid-id "x"))

(defn get-y [grid-id]
  (number-after grid-id "y"))

(defn cell->obj [line-as-cells]
  (let [[one two three four five] line-as-cells]
    {:grid-id one
     :x (get-x one)
     :y (get-y one)
     :used    (->> three
                   (all-but-last)
                   (u/string->int))
     :avail   (->> four
                   (all-but-last)
                   (u/string->int))}))

;;
;;
;;
(defn make-obj [line]
  (let [line-as-cells (string/split (string/trim line) #"\s+")
        ;_ (println cells)
        ]
    (cell->obj line-as-cells)))

;any two nodes (A,B), regardless of whether they are directly connected, such that:
;Node A is not empty (its Used is not zero).
;Nodes A and B are not the same node.
;The data on node A (its Used) would fit on node B (its Avail).
(defn spare-space-in-second [node-a node-b]
  (let [a-used (:used node-a)
        b-avail (:avail node-b)]
    (>= b-avail a-used)))

(defn too-big-to-move? [node]
  (>= (:used node) 28))

;;
;; Does a have any used data to move?
;; And can we move a's used data into avail space in b?
;;
(defn viable-pairs [objects]
  (for [a objects
        b objects
        :let [space? (spare-space-in-second a b)]
        :when (and space? (not= a b) (-> a :used zero? not))
        ]
    [a b]))

(defn part-one []
  (let [
        raw-input (slurp "./advent/twenty_two.txt")
        ;raw-input steps
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        raw-df-lines (drop 2 in)
        objects (mapv make-obj raw-df-lines)
        pairs (viable-pairs objects)]
    (count pairs)
    ))

(defn gridify [row-width v]
  (let [parted (partition row-width v)]
    (u/transpose parted)))

(defn breadth-first-search [max-steps
                            starting-grid
                            generate-possibilities
                            destination-state?]
  (loop [already-tested #{starting-grid}
         last-round #{starting-grid}
         times 0
         most-distant-candidates []]
    (assert (set? already-tested))
    (if (< times max-steps)
      (let [_ (println "steps done:" times "visited:" (count already-tested))
            newly-generated (mapcat generate-possibilities last-round)]
        (if (seq newly-generated)
          (let [got-there? (first (filter destination-state? newly-generated))]
            (if got-there?
              (do
                ;(println (str "Got there with: <" got-there? ">"))
                {:steps (inc times) :res got-there?})
              (let [now-tested (into already-tested newly-generated)]
                (recur now-tested (into #{} (remove already-tested newly-generated)) (inc times) most-distant-candidates))))
          [:dead-end "Nowhere to go, not even back where came from" already-tested]))
      [:need-more-steps "Need give more steps then run again" already-tested])))

;;
;; We filter out any differences that are 0, or land you at an immovable.
;; immovables s/be a vector of x y vectors.
;;
(defn swap-steps [width height immovables node]
  (let [
        ;; diff gives the movement required to get the node nearer to the target. For instance
        ;; if x-diff is positive then required movement will be to the right
        ;; But we don't care about diff as going in all possible directions
        current-place [(:x node) (:y node)]
        [curr-x curr-y] current-place
        ;x-diff (- x curr-x)
        ;y-diff (- y curr-y)
        new-possible-places [[curr-x (+ curr-y 1)]
                             [(+ curr-x 1) curr-y]
                             [curr-x (- curr-y 1)]
                             [(- curr-x 1) curr-y]]
        out-of-bounds-fn (fn [[x y]] (or (neg? x) (neg? y) (>= x width) (>= y height)))
        new-places (remove out-of-bounds-fn new-possible-places)
        not-good-places (into #{} (conj immovables current-place))
        good-places (remove not-good-places new-places)
        good-moves (map (fn [to-place] [to-place current-place]) good-places)
        ]
    good-moves))

(defn replace-node-in-grid [grid new-node [x y]]
  (assert (vector? grid))
  (assert (vector? (first grid)))
  (assoc-in grid [y x] new-node)
  )

(defn node-size [node]
  (+ (:used node) (:avail node)))

(defn get-grid [grid x y]
  (get-in grid [y x]))

;(defn swap-nodes [grid [from-x from-y] [to-x to-y]]
;  (let [from-node (get-grid grid from-x from-y)
;        to-node (get-grid grid to-x to-y)]
;    (-> grid
;        (assoc-in [from-y from-x] to-node)
;        (assoc-in [to-y to-x] from-node))))

;;
;; move is from one coordinate to another. We move all of what is :used in the-from-node to the-to-node,
;; thus increasing the to node's used. We must also adjust :avail up in the from, and down in the
;; to node. If avail in to node becomes -ive then need to crash.
;; This is moving the data, and leaving the from node with lots of free space.
;; In theory we don't need to do this, but seems less work to work with the orig data structures
;;
(defn move-grid [grid [[from-x from-y] [to-x to-y]]]
  (let [_ (assert (or (not= from-x to-x) (not= from-y to-y)))
        orig-from-node (get-grid grid from-x from-y)
        _ (assert orig-from-node (str "No node found at " from-x ", " from-y " in " grid))
        orig-to-node (get-grid grid to-x to-y)
        _ (assert orig-to-node)
        transfer-amount (:used orig-from-node)
        _ (assert (pos? transfer-amount) (str "Nothing to transfer: " transfer-amount " from " orig-from-node))
        ;; new to node is going to have more used and less avail
        new-to-node (assoc orig-to-node :used (+ transfer-amount (:used orig-to-node))
                                        :avail (- (:avail orig-to-node) transfer-amount))
        _ (assert (not= orig-to-node new-to-node) (str orig-to-node "," new-to-node))
        ;; new from node is going to have less used and more avail
        new-from-node (assoc orig-from-node :used 0
                                            :avail (+ (:avail orig-from-node) transfer-amount))
        _ (assert (not= orig-from-node new-from-node))
        _ (assert (#(-> % neg? not) (:avail new-to-node)) (str "Can't leave to node with less than nothing available: " new-to-node))
        new-grid (-> grid
                     (assoc-in [from-y from-x] new-from-node)
                     (assoc-in [to-y to-x] new-to-node))
        ]
    new-grid))

(defn grid->grids [moves grid]
  #_(for [move moves
        :let [now-moved (move-grid grid move)]]
    now-moved)
    (let [_ (println "count moves " (count moves))
          f (partial move-grid grid)
          res (map f moves)]
      res)
  )

;;
;; one grid in, many out
;; Only going for next place in the path, which will do many times as crawl across, dodging any #
;; bfs is brute force, generating many changes for each that is accomodating, and there are many
;; that are accomodating.
;;
(defn gen-possibilities [width height]
  (fn [grid]
    (let [all-flat (flatten grid)
          top-right (-> grid first last)
          spare-space-fn (partial spare-space-in-second top-right)
          carriers (filter spare-space-fn all-flat)
          _ (println "carriers: " carriers)
          immovables (filter too-big-to-move? all-flat)
          _ (println "immovables: " immovables)
          move-possibilities (for [carrier carriers
                                   :let [steps (swap-steps width height immovables carrier)]]
                               steps)
          moves (mapcat identity move-possibilities)
          _ (println "generated moves: " moves)
          res (grid->grids moves grid)
          ]
      res)))

(def row-width 3)
(def column-height 3)

(defn x-3 []
  (let [raw-input (slurp "./advent/twenty_two_example.txt")
        ;raw-input steps
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        raw-df-lines (drop 2 in)
        objects (mapv make-obj raw-df-lines)
        grid (gridify row-width objects)
        ;top-right (-> grid first last)
        ;spare-space-fn (partial spare-space-in-second top-right)
        ;all-flat (flatten grid)
        ;carriers (filter spare-space-fn all-flat)
        ;immovables (filter too-big-to-move? all-flat)
        possibilities ((gen-possibilities row-width column-height) grid)
        ]
    (first possibilities)))

(defn x-1 []
  (let [res (swap-steps row-width column-height [[1 1]] {:grid-id "/dev/grid/node-x1-y1", :x 1, :y 2, :used 0, :avail 8})]
    res))

(defn x []
  (let [raw-input (slurp "./advent/twenty_two_example.txt")
        ;raw-input steps
        in (line-seq (BufferedReader. (StringReader. raw-input)))
        raw-df-lines (drop 2 in)
        objects (mapv make-obj raw-df-lines)
        grid (gridify row-width objects)
        _ (pp/pprint (str "GOAL: " (get-grid grid 2 0)))
        _ (pp/pprint (str "space?: " (get-grid grid 1 1)))
        space-required (:used (get-grid grid 2 0))
        _ (println "space-required:" space-required)
        res (breadth-first-search 10 grid (gen-possibilities row-width column-height) (fn [grid] (>= (:avail (get-grid grid 1 0)) space-required)))
        ;_ (println "first count:" (:steps res))
        new-grid (move-grid (:res res) [[2 0] [1 0]])
        ;res-res (breadth-first-search 10 (:res res) (gen-possibilities row-width column-height) (fn [grid] (>= (:avail (get-grid grid 0 0)) space-required)))
        ;_ (println "second count:" (:steps res-res))
        ]
    (pp/pprint res)
    (pp/pprint new-grid)))

(def start-grid [[\. \. \G]
                 [\. \_ \.]
                 [\# \. \.]])

(defn end-grid? [grid]
  (= (-> grid first second) \G))