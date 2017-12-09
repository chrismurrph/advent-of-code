(ns advent-2017.day09
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def test-input-scores-1
  [["{}" 1]
   ["{{{}}}" 6]
   ["{{},{}}" 5]
   ["{{{}}!}}},{}},{{}}}}" 16]
   ["{{<{}!}>}!}}},{}},{{}}}}" 9]
   ["{{{},{<>},{{}}}}" 16]
   ["{{{},<{}>,{{}}}}" 13]
   ["{{{}<,{},>{{}}}}" 13]
   ["{<{},{},{{}}>}" 1]
   ["{<a>,<a>,<a>,<a>}" 1]
   ["{{<a>},{<a>},{<a>},{<a>}}" 9]
   ["{{<!>},{<!>},{<!>},{<a>}}" 3]
   ["{{<!!>}}" 3]
   ["{<!!!>}>}" 1]
   ["{{<!!!>}>}}" 3]
   ["{{<!!>}>}}" 3]
   ["{{<!>!},{<!>},{<!>},{<a>}}" 3]

   ["{{<<!>!},{<!>},{<!>!},{<a>>}}" 3]

   ;["{<\",{i>}" 1]
   ;["{{{{<!>{!!\"u!>},<a}o!!<!>,<a!!\"e>}}},{}},{<\",{i>}" 1]
   ])

(def test-input-scores-2
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

(def test-input-scores test-input-scores-2)

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
        {:depth new-depth
         :score (+ score points)
         :idx (inc idx)}))
    {:depth 0
     :score 0
     :idx   0}
    toks))

(defn consume [[[head & tail] output within-garbage? prior-contig-bangs-count]]
  (let [cancel-this-by-last-bang? (odd? prior-contig-bangs-count)
        start-of-garbage? (and (not cancel-this-by-last-bang?) (not within-garbage?) (= \< head))
        new-within-garbage? (or start-of-garbage?
                                (and within-garbage?
                                     (or cancel-this-by-last-bang? (not= \> head))))
        contig-bangs-count (if (= \! head)
                             (inc prior-contig-bangs-count)
                             0)
        tok {:ch             head
             :start-garbage? start-of-garbage?
             :end-garbage?   (and (= head \>) within-garbage? (not cancel-this-by-last-bang?))
             :start-group?   (and (= \{ head) (not new-within-garbage?))
             :end-group?     (and (= \} head) (and (not cancel-this-by-last-bang?)
                                                   (not new-within-garbage?)))
             }
        new-output (conj output tok)]
    [tail new-output new-within-garbage? contig-bangs-count]))

(defn ending-state? [[input _ _ _]]
  (empty? input))

(defn input->toks [in]
  (->> (iterate consume [(seq in) [] false 0])
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

(defn x-1 []
  (->> (get-input)
       dev/probe-count-off
       score-input))

(defn x-2 []
  (->> test-input-scores
      (drop 4)
      (take 1)
      ffirst
      input->toks
      dev/probe-on
      reduce-score))

(defn x-3 []
  (-> (nth test-input-scores 6)
      first
      input->toks
      dev/probe-on
      reduce-score))
