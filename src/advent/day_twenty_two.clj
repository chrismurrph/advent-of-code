(ns advent.day-twenty-two
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.data :as data]
            [clojure.data.priority-map :as pm]))

(def df-node #"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%")

(defn read-node [line]
  (let [[_ x y size used avail percent] (re-find df-node line)]
    {:x (Integer/parseInt x)
     :y (Integer/parseInt y)
     :size (Integer/parseInt size)
     :used (Integer/parseInt used)
     :avail (Integer/parseInt avail)
     :percent (Integer/parseInt percent)}))

(defn node-empty? [node]
  (zero? (:percent node)))

(defn node-not-empty? [node]
  (not (node-empty? node)))

(defn viable-pairs [nodes]
  (for [a nodes
        b nodes
        :when (and (not= a b)
                   (node-not-empty? a)
                   (<= (:used a) (:avail b)))]
    [a b]))

(defn solve-one []
  (let [nodes (->> "day_22.txt"
                   io/resource
                   slurp
                   s/split-lines
                   (drop 2)
                   (map read-node))]
    (count (viable-pairs nodes))))

(def real-data (slurp (io/resource "2016/twenty_two.txt")))

(defn nodes []
  (let [nodes (->> real-data
                   s/split-lines
                   (drop 2)
                   (map read-node))]
    nodes))

(defn test-nodes []
  (let [nodes (->> "Filesystem            Size  Used  Avail  Use%
/dev/grid/node-x0-y0   10T    8T     2T   80%
/dev/grid/node-x0-y1   11T    6T     5T   54%
/dev/grid/node-x0-y2   32T   28T     4T   87%
/dev/grid/node-x1-y0    9T    7T     2T   77%
/dev/grid/node-x1-y1    8T    0T     8T    0%
/dev/grid/node-x1-y2   11T    7T     4T   63%
/dev/grid/node-x2-y0   10T    6T     4T   60%
/dev/grid/node-x2-y1    9T    8T     1T   88%
/dev/grid/node-x2-y2    9T    6T     3T   66%"
                   s/split-lines
                   (drop 1)
                   (map read-node))]
    nodes))

(defn max-coords [nodes]
  [(apply max (map :x nodes))
   (apply max (map :y nodes))])

(def max-x 32)

(def max-y 29)

(defn build-graph [nodes]
  (reduce (fn [acc {:keys [x y] :as node}]
            (assoc acc [x y] node)) {} nodes))

(defn has-capacity? [{:keys [avail]} capacity]
  (<= capacity avail))

(def adjacent
  (memoize (fn [[x y]]
             (filter (fn [[x y]]
                       (and (nat-int? x) (nat-int? y)
                            (>= max-x x) (>= max-y y)))
                     [[(inc x) y] [(dec x) y]
                      [x (inc y)] [x (dec y)]]))))

(defn mv [graph n1 n2]
  (let [used (get-in graph [n1 :used])
        size (get-in graph [n1 :size])]
    (-> graph
        (assoc-in [n1 :used] 0)
        (assoc-in [n1 :avail] size)
        (update-in [n2 :used] + used)
        (update-in [n2 :avail] - used))))

(defn next-states [distance coord graph]
  (let [moves (into [] (comp (filter (fn [dest] (has-capacity? (get graph dest) (:used (get graph coord)))))
                             (map (fn [dest] [(inc distance) dest (mv graph coord dest)])))
                    (adjacent coord))]))

(defn viable-adjacent-pairs [nodes]
  (for [a nodes
        b nodes
        :when (and (not= a b)
                   (<= (:used a) (:avail b))
                   (get (into #{} (adjacent [(:x a) (:y a)])) [(:x b) (:y b)]))]
    [a b]))

(def adjacent-to? (memoize (fn adjacent-to? [[x y] [x1 y1]]
                             (get (into #{} (adjacent [x y])) [x1 y1]))))

(defn move-coord [coord a b]
  (if (and (= coord [(:x a) (:y a)])
           (adjacent-to? coord [(:x b) (:y b)]))
    [(:x b) (:y b)]
    coord))

(defn nearby-with-capacity [coords graph used distance path seen]
  (into [] (comp (filter (fn [coord]
                           (let [{:keys [size]} (get graph coord)]
                             (<= used size))))
                 (filter (fn [coord]
                           (nil? (get seen coord))))
                 (map (fn [coord]
                        [(inc distance) coord (conj path coord)])))
        (adjacent coords)))

(defn shortest-path-capacity [graph start dest]
  (let [{:keys [used]} (get graph start)]
    (loop [q [[0 start []]]
           seen #{}]
      (let [[first & rest] q
            [distance coord path] first]
        (cond (nil? first) nil
              (= coord dest) first
              :else (let [next-states (nearby-with-capacity coord graph used distance path seen)] (recur (into (vec rest) next-states)
                                                                                                         (into seen (map second next-states)))))))))

;; Someone else's used goes to that avail
;; adj to empty, used needs to go to size

;; Each node should have graph, coord, path.

(defn nearby-moves [graph coord path seen]
  (let [available (get-in graph [coord :avail])]
    (into [] (comp (filter (fn [c]
                             (<= (get-in graph [c :used])
                                 available)))
                   (filter (fn [c] (nil? (get seen c))))
                   (map (fn [c] [(mv graph c coord)
                                 c
                                 (conj path c)])))
          (adjacent coord))))

(defn open-to-path [graph start dest seen]
  (loop [q [[graph start []]]
         seen seen]
    (let [[first & rest] q
          [g coord path] first]
      (cond (nil? first) nil
            (= coord dest) first
            :else (let [next-states (nearby-moves g coord path seen)]
                    (recur (into (vec rest) next-states)
                           (into seen (map second next-states))))))))

;; Okay, 26 12 is the closest in our case, so we can just walk the path
;; to each node on the path and see if that works

(defn solve-two [graph start dest]
  (let [available-path (last (shortest-path-capacity graph start dest))
        empty-cell [20 6] #_[26 12]]
    (reduce (fn [{:keys [empty-cell g last-coord] :as acc} coord]
              (let [[g coord path] (open-to-path g empty-cell coord #{last-coord})]
                (println path)
                (-> acc
                    (update :steps + (count path))
                    (assoc :g (mv g last-coord coord))
                    (update :steps inc)
                    (assoc :empty-cell last-coord)
                    (assoc :last-coord coord))))
            {:steps 0
             :g graph
             :empty-cell empty-cell
             :last-coord start} available-path)))

;;
;; I don't know how this one works! - this call is not correct
;;
(defn x []
  (count (solve-two (build-graph (nodes)) [36 0] [0 0])))
