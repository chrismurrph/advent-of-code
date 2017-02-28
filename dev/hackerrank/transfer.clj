(ns hackerrank.transfer)

(def in-arg {:a [11 22 33] :b [10 20 30]})
(def out-arg {:a [11] :b [10 20 30 22 33]})

(defn transfer-1 [in n from-k to-k]
  (let [from-sz (-> in from-k count inc)
        takes (for [i (range n from-sz)]
                (get-in in [from-k (dec i)]))
        transfering (clojure.set/difference (set (from-k in)) (set takes))]
    (into {} [[from-k (vec transfering)] [to-k (vec (concat (to-k in) takes))]])))

(defn transfer [in n from-k to-k]
  (let [
        [left right] ((juxt #(subvec % 0 (dec n)) #(subvec % n)) (from-k in))
        ]
    (into {} [[from-k left] [to-k (vec (concat (to-k in) right))]])))

(defn x-1 []
  (transfer in-arg 2 :a :b))


