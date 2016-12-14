(ns advent.fourteen
  (:require [clojure.string :as str]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn five-times? [sub-seq]
  (fn [hash]
    (let [res (str/index-of hash sub-seq)]
      res)))

(defn triple [hash]
  (first (drop-while #(< (count %) 3) (partition-by identity hash))))

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn search [seed]
  (loop [mode :triple
         idx 0
         thousand {}
         result-keys []]
    (let [hash-val (md5 (str seed idx))
          trip? (triple hash-val)]
      (if (= 64 (count result-keys))
        result-keys
        (case mode
          :triple (if trip?
                    (recur :five-in-thousand
                           (inc idx)
                           {:resume-at-idx   (inc idx)
                            :countdown       999
                            :revisit-triples (or (:revisit-triples thousand) [])
                            :sub-seq-fn      (five-times? (apply str (repeat 5 (first trip?))))}
                           result-keys)
                    (let [
                          ;_ (println (str "thou: " thousand))
                          {:keys [revisit-triples resume-at-idx]} thousand
                          ]
                      (if (seq revisit-triples)
                        ;;
                        ;; Because we have triples that have previously been collected, we can just use up the next
                        ;; one to jump to, and keep doing this
                        ;;
                        (let [
                              [head second & tail] revisit-triples
                              _ (println (str "Instead staying at " idx " want to move s/how to " (vec revisit-triples)))
                              _ (assert resume-at-idx (str "No resume recorded in thousand: " (dissoc thousand :revisit-triples)))
                              ]
                          (recur mode resume-at-idx (assoc thousand :revisit-triples tail) result-keys))
                        (recur mode (inc idx) thousand result-keys))))
          :five-in-thousand (let [{:keys [countdown revisit-triples sub-seq-fn resume-at-idx]} thousand
                                  has-five? (sub-seq-fn hash-val)
                                  ]
                              (if (zero? countdown)
                                ;[idx hash-val revisit-triples]
                                (let [-resume-idx (if (seq revisit-triples)
                                                   (ffirst revisit-triples)
                                                   resume-at-idx)]
                                  (recur :triple -resume-idx {:revisit-triples (next revisit-triples)} result-keys))
                                (if has-five?
                                  (let [result-key [(dec resume-at-idx) idx hash-val countdown]]
                                    (recur :triple resume-at-idx {:revisit-triples revisit-triples} (conj result-keys result-key)))
                                  (let [new-revisit-triples (if trip?
                                                              (conj revisit-triples [resume-at-idx trip? hash-val])
                                                              revisit-triples)]
                                    (recur mode
                                           (inc idx)
                                           (-> thousand
                                               (update :countdown dec)
                                               (assoc :revisit-triples new-revisit-triples))
                                           result-keys))))))))))

(def seed "abc")

(defn x-1 []
  (let [c \8
        hash (md5 "abc18")
        trip (triple hash)
        to-search (apply str (repeat 5 (first trip)))
        five-times-sample "00888"
        _ (println to-search)]
    ((five-times? to-search) five-times-sample)))

(defn x []
  (search seed))
