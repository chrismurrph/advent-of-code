(ns advent.fifteen)

;Disc #1 has 5 positions; at time=0, it is at position 2.
;Disc #2 has 13 positions; at time=0, it is at position 7.
;Disc #3 has 17 positions; at time=0, it is at position 10.
;Disc #4 has 3 positions; at time=0, it is at position 2.
;Disc #5 has 19 positions; at time=0, it is at position 9.
;Disc #6 has 7 positions; at time=0, it is at position 0.
(def first-part-discs [{:time-zero-pos 2 :length 5}
                       {:time-zero-pos 7 :length 13}
                       {:time-zero-pos 10 :length 17}
                       {:time-zero-pos 2 :length 3}
                       {:time-zero-pos 9 :length 19}
                       {:time-zero-pos 0 :length 7}])

(def second-part-discs (conj first-part-discs {:time-zero-pos 0 :length 11}))

(def example-discs [{:time-zero-pos 4 :length 5} {:time-zero-pos 1 :length 2}])
(def t 5)

(def discs second-part-discs)

;;
;; n repesents where it is away from (lower than) drop level. First is at 1 b/c 0 is where capsules are dropped from.
;;
(defn thru-at? [t n {:keys [time-zero-pos length]}]
  (let [will-be-at (+ t n time-zero-pos)
        res (rem will-be-at length)]
    (zero? res)))

(defn all-thru? [t discs]
  (map-indexed (fn [idx disc] (thru-at? t (inc idx) disc)) discs))

(defn find-t [discs]
  (loop [time-at 0]
    (let [good-result (every? identity (all-thru? time-at discs))]
      (if good-result
        time-at
        (recur (inc time-at))))))

#_(defn x-1 []
  (let [level 1]
    (thru-at? t level (nth discs (dec level)))))

(defn x-2 []
  (every? identity (all-thru? t discs)))

(defn x-3 []
  (find-t discs))
