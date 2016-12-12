(ns advent.eleven
  (:require [instaparse.core :as insta]
            [clojure.pprint :as pp]))

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

(def example-lab {:elevator [:F1 [[:generator "plutonium"] [:generator "strontium"]]]
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
(def starting-lab {:elevator :F3
                   :F1 [[:generator "thulium"] [:microchip "thulium"] [:generator "plutonium"] [:generator "strontium"]],
                   :F2 [[:microchip "plutonium"] [:microchip "strontium"]],
                   :F3 [[:generator "promethium"] [:microchip "promethium"] [:generator "ruthenium"] [:microchip "ruthenium"]],
                   :F4 []})

(defn x-3 []
  (load-lift-combinations example-floor)
  #_(advent.day11/select-elevator-items example-floor))

;;
;; State transition based on a generated lift load.
;; We need to take from current and put into new floor, and move the lift
;;
(defn move-one [lab to-floor-id lift-load]
  (let [_ (assert (set? lift-load))
        current-floor-id (:elevator lab)
        _ (assert current-floor-id (str "Strange lab no elevator"))
        ]
    (-> lab
        (assoc :elevator to-floor-id)
        (update current-floor-id (fn [floor-content]
                                   ;(println (str "To rm " lift-load " from " floor-content))
                                   (vec (remove lift-load floor-content))))
        (update to-floor-id into lift-load))))

(defn x-4 []
  (let [res (move-one starting-lab :F4 #{[:generator "ruthenium"] [:microchip "ruthenium"]})]
    (println "F4" (:F4 res) "F3" (:F3 res) "elevator:" (:elevator res))))

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
  (let [current-floor (:elevator lab-in)
        ]
    (for [new-floor (next-floors current-floor)
          lift-combo (load-lift-combinations (current-floor lab-in))
          :let [lab-out (move-one lab-in new-floor lift-combo)]
          :when (and (safe-floor? (current-floor lab-out))
                     (safe-floor? (new-floor lab-out)))]
      lab-out)))

(defn x-5 []
  (let [res (generate-possible-moves starting-lab)]
    (pp/pprint res)
    (println (count res))))