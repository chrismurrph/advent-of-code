(ns advent-2017.day07
  (:require [utils :as u]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(def example-line-1 "brexb (75) -> tbmiv")
(def example-line-2 "lovxjut (90) -> fmvna, ddneaes, sakwdmk, lqmoz")
(def example-line-3 "toprb (282) -> dzyvcxt, xlyngh, tkhbr, avufn, uhhiz, tmtqgn")
(def example-line-4 "cfkcj (74)")

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
;; I don't know how to do repeating groups in this regex system
;;
(def regex-1 #"(\S+)( )(\(\d+\))( )?(-> )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?(\S+, )?")
(def regex-2 #"(\S+)( )(\(\d+\))( )(-> )(\S+, )+")

(defn trim-trailing-comma [s]
  (let [idx (s/index-of s ",")]
    (if idx
      (subs s 0 idx)
      s)))

(defn parse [line]
  (->> (re-matches regex-1 (dev/probe-off (if (s/includes? line "->")
                                            (s/join (concat line ", "))
                                            line)))
       dev/probe-off
       rest
       (u/drop-nth 1)
       (u/drop-nth 1)
       (u/drop-nth 1)
       (u/drop-nth 1)
       ;(u/drop-nth 1)
       (map trim-trailing-comma)
       ((fn [line]
          ((juxt first #(-> % next set)) line)))))

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
                (into {}))]
    ;(dev/pp in)
    (->> (tsort in)
         last
         last)))

(defn x-2 []
  (let [in (->> (get-example-input)
                (map parse)
                (into {}))]
    (dev/pp in)
    (->> (tsort in))))

;;
;; TESTS
;;

(deftest depends-on-none
         (is (= ["cfkcj" #{}]
                (parse example-line-4))))

(deftest ignores-when-no
  (is (= "tbmiv"
         (trim-trailing-comma "tbmiv"))))

(deftest parses-okay
  (is (= [["cfkcj" #{}] ["jgtvhkv" #{"gvupyd" "vuyxvq" "ykqcpiv"}]]
         (->> (get-input)
              (take 5)
              (map parse)
              ((juxt first last))))))
