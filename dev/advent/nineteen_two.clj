(ns advent.nineteen-two
  (:require [utils :as u]))

(defn make-eleves [num]
  (for [i (range num)]
    {:elf-num (inc i) :num-presents 1}))

(defn split-victim [tail-list]
  (let [size (count tail-list)
        pivot-pos (if (even? size)
                    (dec (/ size 2))
                    (/ (dec size) 2))
        left-side (take pivot-pos tail-list)
        right-side (drop (inc pivot-pos) tail-list)
        to-select (first (drop pivot-pos tail-list))
        ]
    [left-side to-select right-side]
    ))

;;
;; Works fine, too slow. Most likely because split-victim is doing a huge amount of taking and
;; dropping. Need to avoid taking or dropping any more than one or two
;;
(defn present-turns-slow [starting-eleves]
  (loop [one-list starting-eleves
         index 0]
    (let [[thief & tail] one-list]
      (if tail
        (let [[left victim right] (split-victim tail)
              ;_ (println thief victim left right)
              num-presents-to-take (:num-presents victim)
              happy-thief (update thief :num-presents #(+ % num-presents-to-take))]
          (recur (concat left right (list happy-thief)) (inc index)))
        thief))))

;;
;; If have to make one bigger it is always the right one. If take from both
;; equally then the left is the one will end early, and the right may still have
;; one in it.
;;
(defn split-left-right [list]
  (let [size (count list)
        pivot-pos (if (even? size)
                    (/ size 2)
                    (/ (dec size) 2))
        left-side (take pivot-pos list)
        right-side (drop pivot-pos list)
        ]
    [left-side right-side]
    ))

;;
;; Won't work for even number of elves, but doesn't need to to answer the question!
;; Tried using vectors because they are associative and will work with nth.
;; But that doesn't give us the lazyness we need.
;; So back to having lists and this time need to be only working with the front of them
;; Also don't count on them, as lazy no good for being counted!
;;
(defn present-turns-fast [starting-eleves]
  (let [[left-starting right-starting] (split-left-right starting-eleves)]
    (loop [left-list left-starting
           right-list right-starting
           old-thieves []
           knock-outs #{}
           sequence-idx 0
           index 0]
      (if (>= sequence-idx (count left-list))
        (let [with-knock-outs-removed (remove knock-outs right-list)]
          ;(assert false (str (seq with-knock-outs-removed) "," old-thieves))
          (recur with-knock-outs-removed (seq old-thieves) [] #{} 0 (inc index))
          )
        (let [thief (nth left-list sequence-idx)
              knock-out-idx (if (even? sequence-idx) sequence-idx (inc sequence-idx))]
          (if (< knock-out-idx (count right-list))
            (let [victim (nth right-list knock-out-idx)
                  num-presents-to-take (:num-presents victim)
                  _ (println "thief: " thief)
                  _ (println "victim: " victim)
                  happy-thief (update thief :num-presents #(+ % num-presents-to-take))]
              (recur left-list right-list (conj old-thieves happy-thief) (conj knock-outs victim) (inc sequence-idx) (inc index)))
            thief))))))

(def test-num-eleves 5)
(def num-eleves 3017957)

(defn x []
  (let [eleves (make-eleves test-num-eleves)
        ;_ (println eleves)
        ]
    (present-turns-fast eleves)))
