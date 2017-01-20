(ns advent.eleven
  (:require [utils :as u]
            [medley.core :refer [distinct-by]]
            [clojure.set :refer [difference union intersection]]
            [clojure.math.combinatorics :as combo]
            [clojure.spec :as s]
            ))

;;
;; Bruce below here
;;

;; ::pos is same thing as floor number
(s/def ::pos (s/and integer? (s/or :zero? zero? :pos? pos?)))
(s/def ::generator (s/and integer? pos?))
(s/def ::chip (s/and integer? neg?))
(s/def ::item (s/or :generator ::generator
                    :chip      ::chip))
(s/def ::floor-contents (s/* ::item))
(s/def ::floors (s/map-of ::pos (s/coll-of ::item)))
(s/def ::lab-state (s/keys :req-un [::pos ::floors]))

;; pairs and each pair is for a rock type, and is of form [chip-floor generator-floor].
(s/def ::canonical-state (s/+ (s/tuple ::pos ::pos)))

;;
;; +ive is generator, -ive is chip
;; The actual number is the type of a rock
;;
(def start-state2 {:pos    0
                   :floors {0 #{1 -1 100000 -100000 1000000 -1000000}
                            1 #{10 100 1000 10000}
                            2 #{-10 -100 -1000 -10000}
                            3 #{}}})

(defn x-10 []
  (s/valid? ::lab-state start-state2))

;;
;; Above not yet in canonical form. There are 3 matched pairs on floor 0, hence: [0 0] [0 0] [0 0].
;; Each pair shows where the chip and generator are for an unnamed rock.
;;
;; [0 ([0 0] [0 0] [0 0] [2 1] [2 1] [2 1] [2 1])]
;;
;; Even thou it is the same name this function is not validated that args and return types are correct
;;
(s/fdef canonical
        :args ::floors
        :ret ::canonical-state)

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
        ]
    (-> (reduce (fn [acc [p items]]
                  (reduce #(assoc-in %1 [(Math/abs %2)
                                         (if (pos? %2) 1 0)] p) acc items))
                init floors)
        vals
        sort
        )))

(defn to-canonical [{:keys [pos floors]}]
  [pos (canonical floors)])

(defn x-11 []
  (let [simplest-state (to-canonical start-state2)]
    (println (second simplest-state))
    (s/valid? ::canonical-state (second simplest-state))))

(def state-score #(-> % second meta :score))

;;
;; Example input:
;; ([1 1] [2 0] [2 1] [2 1] [2 1]), where [chip gen]
;; From above can see that floor 1 has 4 gens and one chip, and that the chip is safe, as it is coupled.
;; So floor 1 is safe. Unsafe is when a different generator is on your (you being a chip) floor. Here we
;; have lots of chips on floor 2, but no generators on floor 2 to fry them.
;; In the code below [1 1] is excluded by not=, so chips becomes #{2} and gens #{0 1}
;; So generators and chips are on different floors and the lab is valid, as calling this only for valid
;; elevators.
;;
;; All floors that have generators. All floors where chip is on different floor to its generator.
;; These floors have vulnerable chips. So can't be same as a generator floor.
;; i.e. No generators can be on a floor where there are chips whose generator is else-floor
;;
(def valid-floors?
  (memoize
    (fn [n]
      (let [gens (set (keep second n))
            chips (set (keep first (filter #(apply not= %) n)))]
        (empty? (intersection gens chips))))))

(def counter (atom 0))
(defn true-until [counted]
  (if (= counted @counter)
    false
    (do
      (swap! counter inc)
      true)))

(def generator? odd?)
(def chip? even?)

;;
;; Elevator has 1 or 2 things in it.
;; If both generators or both chips or only one then no radiation
;; Good case is when the generator of the chip (inc chip) is in the other position
;;
(defn safe-elevator? [positions]
  ;(assert (true-until 20) (str "postiions: " (seq positions)))
  (or (= 1 (count positions))
      (every? generator? positions)
      (every? chip? positions)
      (let [chip (some #(when (chip? %) %) positions)]
        ((set positions) (inc chip)))))

(def finished-score 10000000)

(defn finished? [n] (= #{[3 3]} (set n)))

(def score
  (fn [n]
    (if (finished? n)
      finished-score
      (* 10 (count (filter #(= 3 %) (flatten n)))))))

;;
;; Find the indexes (places) where the current floor is
;; Each in flattened is either a chip-floor or a generator-floor
;; Because chip comes first and start at zero, then any even index/place is a chip.
;; So if floor 0 is the one we are on then:
;; flattened: [0 0 2 0 2 1 2 1 2 1]
;; flr: 0
;; places: (0 1 3)
;; So on floor 0 we have one chip (at 0) and two generators (at 1 and 3)
;; Note that the chip at idx 0 is safe because there is a generator at idx 1.
;; positions' gives us all the safe elevators. For each of these safe elevators we must
;; take these 1 or 2 from the -from- floor and add them to the -to- floor.
;; The reduce puts a generator or a chip onto a different level. The data structure used
;; means we don't have to add and remove.
;; Remember that one move will look like this:
;; [1 ([1 1] [2 0] [2 1] [2 1] [2 1])]
;; distinct-by takes away same generation where lift is on different levels.
;; Every valid move is given a score, with more items on floor three being better
;;
(defn next-possible-states [[flr pairings]]
  (let [
        ;_ (println pairings)
        flattened (vec (flatten pairings))
        ;_ (println flattened)
        lower-bound (reduce min flattened)
        ;_ (println flr)
        places (u/indexes-by #(= % flr) flattened)
        ;_ (println places)
        positions' (->> (map list places)
                        (concat (combo/combinations places 2))
                        (filter safe-elevator?))
        ;_ (println positions')
        moves (for [elevator positions'
                    up-or-down [1 -1]
                    :let [next-floor (+ flr up-or-down)]
                    :when (<= lower-bound next-floor 3)]
                [next-floor
                 (->> (reduce #(assoc %1 %2 next-floor) flattened elevator)
                      (partition 2)
                      (map vec)
                      sort)])
        ;_ (println "moves: " moves)
        ]
    (doall
      (sequence
        (comp
          (filter (comp valid-floors? second))
          (distinct-by second)
          (map #(vary-meta % assoc :score (score (second %)))))
        moves))))

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
;; There's going to be a lot of things that are [prev-states state] as `iterate` processes. Just starting off
;; with one, which has no previous states.
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

(def start-state {:pos 0
                  :floors {0 #{1 -1}
                           1 #{10 100 1000 10000}
                           2 #{-10 -100 -1000 -10000}
                           3 #{}}})

;; part 1
;; => 33, however correct answer is 31
;; Explained by level 32 getting the perfect score, and first level being 0.
(defn x-9 []
  (breadth-first-search 300 start-state2))