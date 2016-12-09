(ns advent.discard)

(defn lowest-x [[[x1 y1] [x2 y2]]]
  (min x1 x2))

(defn highest-x [[[x1 y1] [x2 y2]]]
  (max x1 x2))

(defn lowest-y [[[x1 y1] [x2 y2]]]
  (min y1 y2))

(defn highest-y [[[x1 y1] [x2 y2]]]
  (max y1 y2))

(defn x-of [[[x1 y1] [x2 y2]]]
  (assert (= x1 x2))
  x1)

(defn y-of [[[x1 y1] [x2 y2]]]
  (assert (= y1 y2) (str "y-of s/only be called on horizontal lines, not vertical: " [x1 y1] ", " [x2 y2]))
  y1)

(defn horiz-lines-intersect? [[[x1 y1] [x2 y2]] [[x3 y3] [x4 y4]]]
  (and (= y1 y2 y3 y4) true))

(defn lines-cross-point [line1 line2]
  (assert line1)
  (assert line2)
  (let [vert-line (some (fn [[[x1 y1] [x2 y2]]] (when (= x1 x2) [[x1 y1] [x2 y2]])) [line1 line2])
        horiz-line (some #(when (not= % vert-line) %) [line1 line2])]
    (if vert-line
      (do
        (assert horiz-line)
        (when (and (and (>= (x-of vert-line) (lowest-x horiz-line))
                        (<= (x-of vert-line) (highest-x horiz-line)))
                   (and (>= (y-of horiz-line) (lowest-y vert-line))
                        (<= (y-of horiz-line) (highest-y vert-line))))
          [(x-of vert-line) (y-of horiz-line)]))
      (do
        ;;
        ;; The only possible intersection will be a cross, so no need worry here
        ;;
        ;(assert vert-line (str "No vert line from " line1 ", " line2))
        ;(assert (not (horiz-lines-intersect? line1 line2)))
        )
      )
    ;(println vert-line horiz-line)

    ))

(defn place-where-crossed [line all-lines]
  (some #(lines-cross-point line %) all-lines))

(defn x-3 []
  (lines-cross-point [[-1 0] [1 0]] [[0 2] [0 -3]]))
