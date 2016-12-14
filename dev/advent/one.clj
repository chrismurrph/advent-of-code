(ns advent.one
  (:require [clojure.string :as str]
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

(defn x []
  (let [input (slurp "./advent/one.txt")
        instructions (str/split input #", ")
        res (answer instructions)
        [x y] res
        blocks-away (+ (u/abs x) (u/abs y))]
    blocks-away))

(defn x-2 []
  (let [[x y] [151 -100]]
    (+ (u/abs x) (u/abs y))))

(defn x-3 []
  (gen-points [1 1] [5 1]))
