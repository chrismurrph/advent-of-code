(ns advent.thirteen
  (:require [utils :as u]
            [clojure.pprint :as pp]))

(defn dec->binary [n]
  (Integer/toString n 2))

;; x*x + 3*x + 2*x*y + y + y*y
(defn coord->wall? [fav-num]
  (fn [[x y]]
    (let [pre-seed (+ (* x x) (* 3 x) (* 2 x y) y (* y y))
          post-seed (+ fav-num pre-seed)
          as-binary (dec->binary post-seed)
          res (reduce + (map u/string->int as-binary))]
      (odd? res))))

(defn create-row [f width y]
  (for [i (range width)
        :let [wall? (f [i y])]]
    (if wall?
      \#
      \.)))

;;
;; Width and height have to be irrelevant for determining the answer
;;
(def -width 10)
(def -height 7)
(def fav-num 1358)
(def started-loc [1 1])
(def ended-loc [31 39])

(defn visual-validation [f width height]
  (into [] (for [i (range height)]
             (create-row f width i))))

(defn x-1 []
  (let [formula (coord->wall? fav-num)
        ;wall? (formula [0 0])
        ]
    (pp/pprint (visual-validation formula -width -height))))

(defn bfs [start succ stop]
  (if (stop start)
    [0 start]
    (loop [doing #{start}
           visited #{start}
           steps 1]
      (println "steps" steps " visited " (count visited) " doing " (count doing))
      #_(println "visited" visited)
      (let [next (transduce
                   (comp
                     (mapcat succ)
                     (keep (fn [state]
                             (cond
                               (stop state)
                               {:done state}
                               (contains? visited state)
                               nil
                               :else state)))
                     (halt-when :done))
                   conj
                   #{}
                   doing)]
        (if-let [match (:done next)]
          [steps match]
          (recur next
                 (into visited next)
                 (inc steps)))))))

;;
;; This one can't yet ansewr the second part, but the one above can (visited at step 51).
;; So perhaps we really do need a transducer - work out later when finish 11.
;;
(defn breath-first-search [starting-lab generate-possible-moves destination-state?]
  (loop [already-tested #{starting-lab}
         last-round #{starting-lab}
         times 1]
    (let [
          newly-generated (mapcat generate-possible-moves last-round)
          got-there? (first (filter destination-state? newly-generated))]
      (if got-there?
        (do
          (println (str "Got there with: <" got-there? ">"))
          {:steps times})
        (let [now-tested (into already-tested newly-generated)]
          (recur now-tested (into #{} (remove already-tested newly-generated)) (inc times)))))))

(defn stop-loc? [loc]
  (= loc ended-loc))

;;
;; Generate possible places to go to. There are only 4 surrounding places b/c can't go diagonally.
;; Some will be out of bounds and some will be in a wall. Apart from
;; these two categories we return all.
;;
(defn succ-loc? [formula?]
  (fn [[x y]]
    (let [left [(dec x) y]
          right [(inc x) y]
          above [x (dec y)]
          below [x (inc y)]
          candidates [left right above below]
          in-bounds (remove (fn [[x y]] (or (neg? x) (neg? y))) candidates)
          in-open-space (remove formula? in-bounds)]
      in-open-space)))

(defn x-2 []
  (let [formula (coord->wall? fav-num)]
    ((succ-loc? formula) [1 1])))

(defn x []
  (let [formula (coord->wall? fav-num)]
    (breath-first-search started-loc (succ-loc? formula) stop-loc?)))