(ns advent.eleven
  (:require [instaparse.core :as insta]
            [clojure.pprint :as pp]
            [utils :as u]
            [medley.core :refer [distinct-by]]
            [clojure.set :refer [difference union intersection]]
            [advent.day11 :as geez]
            [clojure.math.combinatorics :as combo]
            ))

(def example-in ["The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip."
                 "The second floor contains a hydrogen generator."
                 "The third floor contains a lithium generator."
                 "The fourth floor contains nothing relevant."])

(def input ["The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator."
            "The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip."
            "The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip."
            "The fourth floor contains nothing relevant."])

(def grammar-1
  (insta/parser
    "<S> = sentence
     floor = 'first' | 'second' | 'third' | 'fourth'
     nothing = <'nothing relevant'>
     <word> = #'[a-zA-Z]+'
     generator = word <' generator'>
     microchip = word <'-compatible microchip'>
     <item> = generator | microchip
     <items> = (<' a '> item <','> ?) + ( <' and a '> item ) ?
     <contents> = (items | <' nothing relevant'>) <'.'>
     sentence = <'The '> floor <' floor contains'> contents"))

(defn to-hiccup [s]
  (->> (first (grammar-1 s))
       (insta/transform
         {:floor (fn [x]
                   (case x
                     "first" :F1
                     "second" :F2
                     "third" :F3
                     "fourth" :F4))})))

(def example-floor [[:generator "thulium"] [:microchip "thulium"] [:generator "plutonium"] [:generator "strontium"]])
(def example-chip-fried-floor [[:microchip "thulium"] [:generator "plutonium"] [:generator "strontium"]])
(def example-safe-floor [[:generator "plutonium"] [:generator "strontium"]])

(def example-lab {:e  [:F1 [[:generator "plutonium"] [:generator "strontium"]]]
                  :F1 []
                  :F2 []
                  :F3 []
                  :F4 []})

(defn parse-floors [in]
  (let [f1 #(vec (next (to-hiccup %)))
        f2 #(vector (first %) (vec (next %)))]
    (into {} (mapv (comp f2 f1) in))))

(defn x-1 []
  (parse-floors input))

(defn safe-floor? [floor]
  (let [chips (map second (filter #(= (first %) :microchip) floor))
        generators (into #{} (map second (filter #(= (first %) :generator) floor)))]
    (every? generators chips)))

;;
;; Each possible load has one or two items in it. eg [[:generator "plutonium"] [:generator "strontium"]]
;; or [[:generator "strontium"]] are both lift loads
;;
(defn load-lift-combinations [floor]
  (let [single-fills (set (map (comp set vector) floor))]
    (into single-fills (for [i floor
                             j floor
                             :when (and (not= i j) (safe-floor? [i j]))]
                         (into #{} [i j])))))

(defn x-2 []
  (assert (= [true false true] (map safe-floor? [example-floor example-chip-fried-floor example-safe-floor]))))

;;
;; State does not need to include any contents of elevator, because they are the transitions
;;
(def starting-lab {:e  :F1
                   :F1 #{[:generator "thulium"] [:microchip "thulium"] [:generator "plutonium"] [:generator "strontium"]},
                   :F2 #{[:microchip "plutonium"] [:microchip "strontium"]},
                   :F3 #{[:generator "promethium"] [:microchip "promethium"] [:generator "ruthenium"] [:microchip "ruthenium"]},
                   :F4 #{}})

;;
;; Also on first floor:
;; An elerium generator.
;; An elerium-compatible microchip.
;; A dilithium generator.
;; A dilithium-compatible microchip.
;;
(def second-part-starting-lab {:e  :F1
                               :F1 #{[:generator "thulium"] [:microchip "thulium"] [:generator "plutonium"] [:generator "strontium"]},
                               :F2 #{[:microchip "plutonium"] [:microchip "strontium"]},
                               :F3 #{[:generator "promethium"] [:microchip "promethium"]
                                     [:generator "ruthenium"] [:microchip "ruthenium"]
                                     [:generator "elerium"] [:microchip "elerium"]
                                     [:generator "dilithium"] [:microchip "dilithium"]},
                               :F4 #{}})

(defn x-3 []
  (load-lift-combinations example-floor)
  #_(advent.day11/select-elevator-items example-floor))

;;
;; State transition based on a generated lift load.
;; We need to take from current and put into new floor, and move the lift
;;
(defn move-one [lab to-floor-id lift-load]
  (let [_ (assert (set? lift-load))
        current-floor-id (:e lab)
        _ (assert current-floor-id (str "Strange lab no elevator"))
        ]
    (-> lab
        (assoc :e to-floor-id)
        (update current-floor-id (fn [floor-content]
                                   ;(println (str "To rm " lift-load " from " floor-content))
                                   (vec (remove lift-load floor-content))))
        (update to-floor-id into lift-load))))

(defn x-4 []
  (let [res (move-one starting-lab :F4 #{[:generator "ruthenium"] [:microchip "ruthenium"]})]
    (println "F4" (:F4 res) "F3" (:F3 res) "elevator:" (:e res))))

(def next-floors {:F4 [:F3]
                  :F3 [:F4 :F2]
                  :F2 [:F1 :F3]
                  :F1 [:F2]})

;;
;; Start off with one lab and produce tonnes.
;; There are floors above and below.
;; There are all the possible take outs from the current floor.
;; All possible new labs come from calling move-one.
;; Then remove all that don't cause problems.
;;
(defn generate-possible-moves [lab-in]
  (let [current-floor (:e lab-in)
        ]
    (for [new-floor (next-floors current-floor)
          lift-combo (load-lift-combinations (current-floor lab-in))
          :let [lab-out (move-one lab-in new-floor lift-combo)]
          :when (and (safe-floor? (current-floor lab-out))
                     (safe-floor? (new-floor lab-out)))]
      lab-out)))

(defn allowed? [lab-in lab-out new-floor]
  (and (safe-floor? ((:e lab-in) lab-out))
       (safe-floor? (new-floor lab-out))))

;;
;; Here we are doing the move a lot (product) and seeing if it is allowed. So a lot of new
;; possible allowed states are the returned product.
;; to-floor will mean the one above and below, so really will generate
;; one in, many out, hence mapcat
;;
(defn lab-succ [lab]
  (let [e (:e lab)]
    (doall
      (for [to-floor (next-floors e)
            take-items (load-lift-combinations ((:e lab) lab))
            :let [_ (when (not (seq take-items))
                      (throw (ex-info "WTF??" {:take-items take-items
                                               :state      lab})))

                  lab' (move-one lab to-floor take-items)]
            :when (allowed? lab' e to-floor)]
        lab'))))

(defn x-5 []
  (let [f advent.day11/lab-succ #_generate-possible-moves
        res (f starting-lab)]
    (pp/pprint res)
    (println (count res))))

(defn destination-state? [lab-in]
  (let [{:keys [e F1 F2 F3]} lab-in]
    (and (= e :F4)
         (= F1 F2 F3 #{}))))

;(def stop-at 15)
(def heavy? false)
(defn my-pr-str [labs]
  (if heavy?
    (u/pp-str labs 100)
    (str (count labs))))

(defn breath-first-search [starting-lab generate-possible-moves destination-state?]
  (loop [already-tested #{starting-lab}
         last-round #{starting-lab}
         total-visited 0
         times 1]
    (let [
          ;where-at (remove already-tested last-round)
          newly-generated (mapcat generate-possible-moves last-round)
          _ (println (str "Generated " (my-pr-str newly-generated) " from " (my-pr-str last-round) " at " times))
          got-there? (first (filter destination-state? newly-generated))]
      ;(println (str "Newly generated: " (count newly-generated)))
      (if got-there?
        (let []
          (println (str "Got there with: <" got-there? ">"))
          times)
        (let [now-tested (into already-tested newly-generated)]
          (recur now-tested (into #{} (remove already-tested newly-generated)) (+ total-visited (count last-round)) (inc times)))))))

;;
;; Copied from thegeez: https://github.com/thegeez/clj-advent-of-code-2016
;; Mine above does the same. Actually needed to answer 2nd part of 13.
;;
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
;; I'm on a fair bit of memory (-Xmx4096m) and it dies at 9 when trying second-part-starting-lab
;; Using all of geez stuff!
;;
(defn x-7 []
  (bfs second-part-starting-lab #_(parse-floors input) #_(u/probe-on (geez/retrieve)) geez/lab-succ #_generate-possible-moves destination-state? #_geez/lab-stop?))

;;
;; +ive is generator, -ive is chip
;; The actual number is the type of a rock
;;
(def start-state2 {:pos    0
                   :floors {0 #{1 -1 100000 -100000 1000000 -1000000}
                            1 #{10 100 1000 10000}
                            2 #{-10 -100 -1000 -10000}
                            3 #{}}})
;;
;; Above in canonical form. There are 3 matched pairs on floor 0, hence: [0 0] [0 0] [0 0].
;; Each pair shows where the chip and generator are for an unnamed rock.
;;
;; [0 ([0 0] [0 0] [0 0] [2 1] [2 1] [2 1] [2 1])]

;;
;; kys is every type of rock in a set
;; init is a {} keyed by these keys, where every value is [nil nil]
;; For example a mapentry of init might be [1000 [nil nil]]
;; We reduce over each mapentry that has come in (eg. [0 #{1 -1 100000 -100000 1000000 -1000000}])
;; , each time making init (accum) more complete.
;; This nested reduce is not complicated because it is the same acc, and there are items on each floor.
;; acc becomes %1 in assoc-in and %2 is an item, say -100000
;; The [] part of assoc-in reflects the structure of acc, which is structure of init. assoc-in goes into
;; keys of maps and positions (nths) of vectors. So -ive is going to be at position 0, and +ive at position
;; 1. What we are actually putting into the vector is p, the floor. Thus a produced mapentry of 10 [2 1]
;; means that 10 (representing some rock, say dilithium) has its chip on floor 2 and its generator on floor
;; 1. For our initial states only floors 0, 1 and 2 have chips/generators on them.
;;
;; This function is producing pairs and each pair is for a rock type, and is of form [chip-floor generator-floor].
(defn canonical [floors]
  (let [kys (set (map #(Math/abs %) (apply concat (vals floors))))
        init (zipmap kys (repeat [nil nil]))
        _ (println init)]
    (-> (reduce (fn [acc [p items]]
                  (reduce #(assoc-in %1 [(Math/abs %2)
                                         (if (pos? %2) 1 0)] p) acc items))
                init floors)
        vals
        sort
        )))

(defn to-canonical [{:keys [pos floors]}]
  [pos (canonical floors)])

(def state-score #(-> % second meta :score))

(def valid-floors?
  (memoize
    (fn [n]
      (let [gens (set (keep second n))
            chips (set (keep first (filter #(apply not= %) n)))]
        (empty? (intersection gens chips))))))

(def safe-elevator?
  (memoize
    (fn [positions]
      (or (= 1 (count positions))
          (every? odd? positions)
          (every? even? positions)
          (let [chip (some #(when (even? %) %) positions)]
            ((set positions) (inc chip)))))))

(def finished-score 10000000)

(defn finished? [n] (= #{[3 3]} (set n)))

(def score
  (memoize
    (fn [n]
      (if (finished? n)
        finished-score
        (* 10 (count (filter #(= 3 %) (flatten n))))))))


(def next-possible-states
  (memoize
    (fn [[flr pairings]]
      (let [flattened (vec (flatten pairings))
            lower-bound (reduce min flattened)
            places (u/indexes-by #(= % flr) flattened)
            positions' (->> (map list places)
                            (concat (combo/combinations places 2))
                            (filter safe-elevator?))
            moves (for [positions positions'
                        up-or-down [1 -1]
                        :let [next-floor (+ flr up-or-down)]
                        :when (<= lower-bound next-floor 3)]
                    [next-floor
                     (->> (reduce #(assoc %1 %2 next-floor) flattened positions)
                          (partition 2)
                          (map vec)
                          sort)])]
        (doall
          (sequence
            (comp
              (filter (comp valid-floors? second))
              (distinct-by second)
              (map #(vary-meta % assoc :score (score (second %)))))
            moves))))))

;; do one level at a time
(defn breadth-first-level [ordered-state-set]
  (println "level" (count (ffirst ordered-state-set)))
  (println "count" (count ordered-state-set))
  (->> ordered-state-set
       (mapcat (fn [[prev-states state]]
                 (let [last-canonical (second (last prev-states))]
                   (->> (next-possible-states state)
                        (filter #(not= last-canonical (second %)))
                        #_(filter (comp (complement (set prev-states)) second))
                        (map #(vector (conj prev-states state) %))))))
       (distinct-by second)
       (sort-by state-score >)))

(def finished-score 10000000)

;;
;; There's going to be a lot of things that are [prev-states state] as `iterate` processes. Just start off
;; with one.
;;
(defn breadth-first-search [limit start-state]
  (->> (iterate breadth-first-level [[[] (to-canonical start-state)]])
       (take-while #(and (not-empty %)
                         (let [scr (state-score (first %))]
                           (println "-" scr)
                           (not= scr finished-score))))
       (take limit)
       count))

(defn x-8 []
  (to-canonical start-state2))