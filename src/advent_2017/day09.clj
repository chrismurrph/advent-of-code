(ns advent-2017.day09
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def test-input-scores
  [
   ["{}" 1]
   ["{{{}}}" 6]
   ["{{},{}}" 5]
   ["{{{},{},{{}}}}" 16]
   ["{<a>,<a>,<a>,<a>}" 1]
   ["{{<ab>},{<ab>},{<ab>},{<ab>}}" 9]
   ["{{<!!>},{<!!>},{<!!>},{<!!>}}" 9]
   ["{{<a!>},{<a!>},{<a!>},{<ab>}}" 3]
   ])

(defn get-input []
  (->> (io/resource "2017/day09")
       slurp))

(defn reduce-score [toks]
  (reduce
    (fn [{:keys [depth score idx] :as acc} {:keys [start-group? end-group?] :as tok}]
      (assert (number? depth))
      (assert (number? score))
      (assert (not (and start-group? end-group?)))
      (let [points (if start-group? (inc depth) 0)
            new-depth (cond-> depth
                              start-group? inc
                              (and (pos? depth) end-group?) dec)]
        (assert (>= new-depth 0) [acc tok])
        {:idx           (inc idx)
         :depth         new-depth
         :score         (+ score points)
         }))
    {:idx   0
     :depth 0
     :score 0}
    toks))

(defn consume [[[head & tail] output within-garbage? prior-contig-bangs-count garbage-count]]
  (let [cancel-this-by-last-bang? (odd? prior-contig-bangs-count)
        start-of-garbage? (and (not cancel-this-by-last-bang?) (not within-garbage?) (= \< head))
        new-within-garbage? (or start-of-garbage?
                                (and within-garbage?
                                     (or cancel-this-by-last-bang? (not= \> head))))
        bang? (= \! head)
        contig-bangs-count (if bang?
                             (inc prior-contig-bangs-count)
                             0)
        proper-ch-in-garbage? (and within-garbage?
                                   (not= \> head)
                                   (not bang?)
                                   (not (odd? prior-contig-bangs-count)))
        new-garbage-count (cond-> garbage-count
                                  proper-ch-in-garbage? inc)
        tok {:ch            head
             :start-group?  (and (= \{ head) (not new-within-garbage?))
             :end-group?    (and (= \} head) (and (not cancel-this-by-last-bang?)
                                                  (not new-within-garbage?)))
             :garbage-count new-garbage-count
             }
        new-output (conj output tok)]
    [tail new-output new-within-garbage? contig-bangs-count new-garbage-count]))

(defn ending-state? [[input _ _ _]]
  (empty? input))

(defn input->toks [in]
  (->> (iterate consume [(seq in) [] false 0 0])
       (drop-while (complement ending-state?))
       ;;Make sure either do a take or a first
       ;(take 4)
       first
       second
       ;dev/pp
       ;score
       ))

(defn score-input [in]
  (-> in
      input->toks
      reduce-score))

(deftest test-test-scores
  (let [inputs test-input-scores]
    (is (= (map second inputs)
           (map (comp :score score-input first) inputs)))))

(def test-garbage-count
  [
   ["<>" 0]
   ["<random characters>" 17]
   ["<<<<>" 3]
   ["<{!>}>" 2]
   ["<!!>" 0]
   ["<!!!>>" 0]
   ["<{o\"i!a,<{i<a>" 10]
   ])

(deftest test-test-garbage
  (let [inputs test-garbage-count
        results (->> inputs
                     ;(drop 6)
                     ;(take 1)
                     (map first)
                     (map input->toks)
                     (map last)
                     (map :garbage-count))]
    (is (= (map second inputs)
           results))))

;; ans: 10616
(defn x-1 []
  (->> (get-input)
       dev/probe-count-off
       score-input))

;; ans: 501
(defn x-2 []
  (->> (get-input)
       input->toks
       last
       :garbage-count))
