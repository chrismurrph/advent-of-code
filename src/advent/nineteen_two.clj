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
    ;(println left-starting right-starting)
    (loop [left-list left-starting
           right-list right-starting
           old-thieves []
           skipped-victims []
           knock-outs #{}
           sequence-idx 0
           index 0]
      (when true #_(< index 20)
        (if (and (> index 1) (and (empty? left-list) (or (nil? (second old-thieves)) (nil? (second skipped-victims)))))
          ;;(u/assrt false (str (seq left-list) "," (seq right-list) "," old-thieves "," skipped-victims))
          (if (= 1 (count right-list))
            (let [others (concat old-thieves skipped-victims)
                  _ (assert (= 1 (count others)) (str "others: " (count old-thieves) ", " (count skipped-victims)))
                  _ (println (str "END: " (count left-list) "," (count right-list) "," (count old-thieves) "," (count skipped-victims)))
                  last-victim (first others)
                  num-presents-to-take (:num-presents last-victim)
                  happiest-thief (update (first right-list) :num-presents #(+ % num-presents-to-take))
                  ]
              happiest-thief
              ;(u/assrt false (str (seq left-list) "," (seq right-list) "," old-thieves "," skipped-victims))
              )
            (let [new-circle (concat skipped-victims left-list right-list old-thieves)
                  [new-left new-right] (split-left-right new-circle)
                  ;_ (println "new" new-left new-right)
                  ]
              (recur new-left new-right [] [] #{} 0 (inc index))))
          (let [thief (first left-list)
                _ (assert thief (str "No thief from <" (seq left-list) ">"))
                ;knock-out-idx (if (even? sequence-idx) sequence-idx (inc sequence-idx))
                drop-count (if (even? sequence-idx) 0 1)
                ]
            (let [victim (first (drop drop-count right-list))
                  ;_ (assert victim (str "No victim from " (seq right-list)))
                  ]
              (if (nil? victim)
                ;(assert false (str "BAD: " (seq left-list) "," (seq right-list) "," old-thieves "," skipped-victims))
                (let [new-circle (concat skipped-victims left-list right-list old-thieves)
                      [new-left new-right] (split-left-right new-circle)
                      ;_ (println "new" new-left new-right)
                      ]
                  (recur new-left new-right [] [] #{} 0 (inc index)))
                (let [num-presents-to-take (:num-presents victim)
                      ;_ (println "thief:" thief)
                      ;_ (println "victim:" victim)
                      happy-thief (update thief :num-presents #(+ % num-presents-to-take))]
                  (recur (drop 1 left-list)
                         (drop (inc drop-count) right-list)
                         (conj old-thieves happy-thief)
                         (if (= 1 drop-count)
                           (do
                             ;(println "where is 4 (1 drop): " right-list)
                             (conj skipped-victims (first right-list)))
                           (do
                             ;(println "where is 4 (0 drop): " right-list)
                             skipped-victims))
                         (conj knock-outs victim)
                         (inc sequence-idx)
                         (inc index)))))))))))

(def test-num-eleves 5)
(def num-eleves 3017957)

;;
;; Not working. To fix manually do 6 (and maybe 7) to check where automated answer goes wrong.
;;
(defn x []
  (let [eleves (make-eleves num-eleves)
        ;_ (println eleves)
        ]
    (present-turns-fast eleves)))
