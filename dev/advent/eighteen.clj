(ns advent.eighteen)

(def input "...^^^^^..^...^...^^^^^^...^.^^^.^.^.^^.^^^.....^.^^^...^^^^^^.....^.^^...^^^^^...^.^^^.^^......^^^^")

(def trap-ch \^)
(def safe-ch \.)

;; Its left and center tiles are traps, but its right tile is not.
;; Its center and right tiles are traps, but its left tile is not.
;; Only its left tile is a trap.
;; Only its right tile is a trap.
(defn trap? [[left centre right]]
  (let [left-is-trap (= left trap-ch)
        right-is-trap (= right trap-ch)
        centre-is-trap (= centre trap-ch)]
    (if (or (and left-is-trap centre-is-trap (not right-is-trap))
            (and centre-is-trap right-is-trap (not left-is-trap))
            (and left-is-trap (not centre-is-trap) (not right-is-trap))
            (and right-is-trap (not centre-is-trap) (not left-is-trap)))
      trap-ch
      safe-ch)))

(defn new-line [old-line]
  (let [annexed-line (str safe-ch old-line safe-ch)
        parted (partition 3 1 annexed-line)
        below-line (map trap? parted)]
    (apply str below-line)))

(defn safe-one-line-count [line]
  (reduce
    (fn [acc ele]
      (if (= ele safe-ch)
        (inc acc)
        acc))
    0
    line))

(defn safe-lines-count [lines]
  (reduce
    (fn [acc ele]
      (+ acc (safe-one-line-count ele)))
    0
    lines))

(defn x []
  (let [test [input (new-line input)]]
    (safe-lines-count test)))

(defn x-1 []
  (let [lines (take 400000 (iterate new-line input))]
    (safe-lines-count lines)))
