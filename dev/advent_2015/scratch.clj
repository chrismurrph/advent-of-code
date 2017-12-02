(ns advent-2015.scratch)

(defn mapset [f v]
  (loop [[head & tail] v
         result-set (hash-set)]
    (if (nil? head)
      result-set
      (let [res (f head)]
        (recur tail (conj result-set res))))))

(defn x-1 []
  (mapset inc [1 1 2 2]))
