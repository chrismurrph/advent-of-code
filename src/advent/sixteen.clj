(ns advent.sixteen)

(defn dragon [in]
  (assert (string? in))
  (let [reversed (reverse in)
        replaced (map (fn [bit]
                        (if (= bit \0)
                          \1
                          (when (= bit \1)
                            \0))) reversed)]
    (apply str in \0 replaced)))

;;
;; Keep calling dragon until reach (usually overfill) the len requirement
;;
(defn fill-disk [initial-state len]
  (loop [acc initial-state]
    (let [overreached-by (- (count acc) len)]
      (cond
        (neg? overreached-by)
        (let [drag-res (dragon acc)]
          (recur drag-res))

        (pos? overreached-by)
        (let [trimmed (apply str (take len acc))]
          trimmed)

        (zero? overreached-by)
        acc))))

(defn two->one [two-part]
  (assert (= 2 (count two-part)))
  (let [[l r] two-part]
    (if (= l r)
      \1
      \0)))

(defn produce-half-again [in]
  (let [res (map two->one (partition 2 in))]
    (if (even? (count res))
      (produce-half-again res)
      (apply str res))))

(defn calc-checksum [in]
  (let [chksum (produce-half-again in)]
    [in chksum]))

(defn x-1 []
  (second (calc-checksum (fill-disk "00111101111101000" 35651584))))

(defn x-2 []
  (produce-half-again "110010110100"))
