(ns advent.nineteen-two
  (:require [utils :as u]))

(defn make-eleves [num]
  (for [i (range num)]
    {:elf-num (inc i) :num-presents 1}))

(defn pick-central [right left-vector]
  (let [
        ;coll (concat right left-vector)
        _ (println "right left-vector" right left-vector)
        size (+ (count right) (count left-vector))
        pivot-pos (if (even? size)
                    (dec (/ size 2))
                    (/ (dec size) 2))
        pivot (- pivot-pos (count left-vector))
        _ (println "pivot: " pivot)
        left-side (take pivot right)
        right-side (drop (inc pivot) right)
        to-select (first (drop pivot right))
        _ (println "left-side to-select right-side" left-side to-select right-side)
        left-and-right (concat left-side right-side)
        ;_ (assert (= 1 (- (count coll) (count left-and-right))))
        ]
    [to-select left-and-right]))

(defn select-victim [thief left-vector right-tail]
  (let [pick (pick-central right-tail left-vector)]
    ;(println "central of:" thief right-tail left-vector "is" (first pick))
    pick))

(defn present-turns [starting-eleves]
  (loop [left-vector []
         right-list starting-eleves
         index 0]
    (when (< index 4)
      (let [[thief & right-tail] right-list
            [victim victim-removed] (select-victim thief left-vector right-tail)]
        (if victim
          (let [num-presents-to-take (:num-presents victim)
                happy-thief (update thief :num-presents #(+ % num-presents-to-take))
                ]
            (recur (conj left-vector happy-thief) victim-removed (inc index)))
          (assert false "No victim")
          #_(let [[one two] left-vector]
            (if two
              (if (nil? thief)
                (recur [] (seq left-vector) (inc index))
                (recur [] (cons thief (seq left-vector)) (inc index)))
              (if (empty? right-list)
                one
                (recur [] (concat victim-removed left-vector) (inc index))))))))))

(def test-num-eleves 5)
(def num-eleves 3017957)

(defn x []
  (let [elves (make-eleves test-num-eleves)]
    (present-turns elves)))
