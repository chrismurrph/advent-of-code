(ns advent-2017.day07
  (:require [utils :as u]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.set :as set]))

(defn get-example-input []
  (->> (io/resource "2017/day07_example")
       slurp
       s/split-lines
       ))

(defn get-input []
  (->> (io/resource "2017/day07")
       slurp
       s/split-lines
       ))

;;
;; I don't know how to do repeating groups in Clojure's regex system
;;
(def regex-1 #"(\S+)( )(\(\d+\))( )?(-> )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?")
(def regex-2 #"(\S+)( )(\(\d+\))( )(-> )(\S+, )+")

(defn bracketed-number->int [s]
  ((comp #(Integer/parseInt %) u/strip-surrounding-brackets) s))

(defn parse [line]
  (->> (re-matches regex-1 (if (s/includes? line "->")
                             (s/join (concat line ", "))
                             line))
       next
       vec
       ((partial u/remove-indexes [1 3 4]))
       (keep u/trim-trailing-comma)
       ((fn [line]
          ((juxt first (fn [line]
                         ((juxt (fn [line]
                                  (-> line second bracketed-number->int))
                                #(-> % next next set)) line))) line)))))

;;
;; Gives 'compile order', so last one will be root of tree
;;
(defn tsort [m]
  (let [depth (fn depth [x]
                (if (empty? (m x))
                  0
                  (->> x m (map depth) (apply max) inc)))]
    (map val (sort-by key (group-by depth (keys m))))))

;; ans: bsfpjtc
(defn x-1 []
  (let [in (->> (get-input)
                (map parse)
                (map (fn [[k v]]
                       [k (second v)]))
                (into {}))]
    ;(dev/pp in)
    (->> (tsort in)
         last
         last)))

(defn x-1-example []
  (let [in (->> (get-example-input)
                (map parse)
                (map (fn [[k v]]
                       [k (second v)]))
                (into {}))]
    (dev/pp in)
    (tsort in)))

;;
;; The structure is an inverted pyramid. This fn will give the weight of the
;; program and all programs on top of it. Could (theoretically) benefit from
;; memoization, as I'm manually doing 2nd top, then 3rd top etc...
;;
(defn program-weight-hof [m]
  (fn inner [program-name]
    (let [[weight disk-program-names] (get m program-name)]
      (let [program-weights (map #(second (inner %)) disk-program-names)
            disk-weight (reduce + 0 program-weights)]
        [weight (+ weight disk-weight)]))))

;;
;; Idea was to keep increasing nth, starting at 1. As it happened 2 gave us the answer. To deal
;; with any data we will just need to reduce over (range). Tomorrow...
;;
(defn highest-possibly-unbalanced [top-sorted]
  ;; The first group doesn't depend on anything (no discs). May need to go for 2nd 3rd etc later...
  (nth top-sorted 2))

(defn ameliorate-group-hof [find-weight]
  (fn [group]
    (let [group (assoc group :item-weights (->> (:programs group)
                                                (map (juxt identity #(find-weight %)))
                                                (into {})))]
      group)))

(defn unbalanced? [group]
  (let [weights (->> group :item-weights vals (map second))]
    (not (apply = weights))))

(defn form-groups [m]
  (->> m
       (map (fn [[k [weight items]]]
              {:head   k
               :weight weight
               :programs  items}))))

;;
;; ans:
;; ({:head "lahahn",
;;   :weight 2750,
;;   :programs #{"fzvctf" "utnrb" "bbytzn"},
;;   :item-weights {"fzvctf" [85 1951], "utnrb" [538 1960], "bbytzn" [1597 1951]}})
;; So "utnrb" is too heavy by 9, which gives answer of 529
;;
(defn x-2 []
  (let [parsed-in (->> (get-input)
                       (map parse))
        parsed-in-m (into {} parsed-in)
        no-weights-map (->> parsed-in
                            (map (fn [[k v]]
                                   [k (second v)]))
                            (into {}))
        top-sorted (tsort no-weights-map)
        candidates (set (highest-possibly-unbalanced top-sorted))
        ;; All groups for which all the members are in candidates. This gives
        ;; us the top level but one groups (in the first instance).
        groups (->> (form-groups parsed-in-m)
                    dev/probe-off
                    ;; No spillover: all programs can be found in candidates
                    (filter #(empty? (set/difference (:programs %) candidates)))
                    (filter #(-> % :programs seq)))
        collect-info (ameliorate-group-hof (program-weight-hof parsed-in-m))
        results (->> groups
                     (map collect-info)
                     dev/probe-off
                     (filter unbalanced?))]
    ;(dev/pp parsed-in-m)
    ;(dev/first-crash-if-more results)
    (dev/pp results)
    ))

;;
;; TESTS
;;

(def example-line-1 "brexb (75) -> tbmiv")
(def example-line-2 "cfkcj (74)")

(deftest weight-from-bracketed
  (is (= 74
         (bracketed-number->int "(74)"))))

(deftest depends-on-none
  (is (= ["cfkcj" [74 #{}]]
         (parse example-line-2))))

(deftest depends-on-one
  (is (= ["brexb" [75 #{"tbmiv"}]]
         (parse example-line-1))))

(deftest parses-okay
  (is (= [["cfkcj" [74 #{}]]
          ["jgtvhkv" [1885 #{"gvupyd" "vuyxvq" "ykqcpiv"}]]]
         (->> (get-input)
              (take 5)
              (map parse)
              ((juxt first last))))))
