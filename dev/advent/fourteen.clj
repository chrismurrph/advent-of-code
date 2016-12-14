(ns advent.fourteen
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn decent-hash? [hash]
  hash)

(defn five-times? [sub-seq]
  (assert (= 5 (count sub-seq)) (str "Not length 5: <" sub-seq ">"))
  (assert (= 1 (count (frequencies sub-seq))))
  (fn [hash]
    (assert (decent-hash? hash))
    (str/index-of hash sub-seq)))

(defn triple [hash]
  (first (drop-while #(< (count %) 3) (partition-by identity hash))))

(defn- -md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(def md5 (memoize -md5))

(defn update-triples [maybe-triple idx triples]
  (let [
        ;_ (println "at" idx (count triples))
        old-removed (remove (fn [{:keys [index]}]
                              (let [age (- idx index)]
                                (>= age 1000))) triples)
        new-added (if maybe-triple
                    (conj old-removed maybe-triple)
                    old-removed)]
    new-added))

(defn triple-remover [five-timers-orig-indexes]
  (assert (set? five-timers-orig-indexes))
  (fn [{:keys [index]}]
    (five-timers-orig-indexes index)))

(defn search-2 [seed]
  (loop [idx 0
         result-keys []
         triples []]
    (let [hash-val (md5 (str seed idx))
          _ (assert (decent-hash? hash-val) (str "Did not manage to get a hash: " seed ", " idx))
          triple? (triple hash-val)]
      (if (> (count result-keys) 64)
        result-keys
        (let [new-triple (when triple?
                           {:triple triple?
                            :index  idx
                            :hash   hash-val
                            :five-times-fn (five-times? (apply str (repeat 5 (first triple?))))})
              updated-triples (update-triples new-triple idx triples)
              ;_ (println updated-triples)
              found-five-timers (reduce
                                  (fn [acc {:keys [five-times-fn index hash]}]
                                    (let [res? (five-times-fn hash-val)]
                                      (if res?
                                        (let [new-acc (conj acc {:orig-hash hash
                                                                 :five-times-hash hash-val
                                                                 :orig-index index
                                                                 :found-index idx})
                                              ;_ (println "new-acc: " new-acc)
                                              ]
                                          new-acc)
                                        acc)))
                                  []
                                  updated-triples)
              ;_ (println found-five-timers)
              remover (triple-remover (set (map :orig-index found-five-timers)))
              re-updated-triples (remove remover updated-triples)
              ;_ (println re-updated-triples)
              new-result-keys (concat result-keys found-five-timers)
              ]
          (recur (inc idx) new-result-keys re-updated-triples)
          )))))

(defn search-1 [seed]
  (loop [mode :triple
         idx 0
         thousand {:max-triple-idx 0}
         result-keys []]
    (let [hash-val (md5 (str seed idx))
          _ (assert (decent-hash? hash-val) (str "Did not manage to get a hash: " seed ", " idx))
          trip? (triple hash-val)]
      (if (= 64 (count result-keys))
        result-keys
        (case mode
          :triple (if trip?
                    (recur :five-in-thousand
                           (inc idx)
                           {:resume-at-idx   (inc idx)
                            :countdown       999
                            :max-triple-idx  (:max-triple-idx thousand)
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
                              ;[head second & tail] revisit-triples
                              [[index trip?] & tail] (drop-while (fn [[i _ _]] (< i idx)) revisit-triples)
                              _ (println (str "Instead staying at " idx " want to move s/how to " index ", then: " trip?))
                              ;_ (assert resume-at-idx (str "No resume recorded in thousand: " (dissoc thousand :revisit-triples) ", so far: " result-keys))
                              ]
                          (recur :five-in-thousand index (assoc thousand :revisit-triples tail
                                                                         :resume-at-idx   (inc index)
                                                                         :countdown       999
                                                                         :sub-seq-fn      (five-times? (apply str (repeat 5 (first trip?))))) result-keys))
                        (recur mode (inc idx) thousand result-keys))))
          :five-in-thousand (let [{:keys [countdown revisit-triples sub-seq-fn resume-at-idx max-triple-idx]} thousand
                                  _ (assert max-triple-idx)
                                  _ (assert sub-seq-fn)
                                  has-five? (sub-seq-fn hash-val)
                                  ]
                              (if (zero? countdown)
                                ;[idx hash-val revisit-triples]
                                (let [-resume-idx (if (seq revisit-triples)
                                                   (ffirst revisit-triples)
                                                   resume-at-idx)]
                                  (recur :triple -resume-idx {:revisit-triples revisit-triples :max-triple-idx max-triple-idx} result-keys))
                                (if has-five?
                                  (let [_ (assert (decent-hash? hash-val))
                                        result-key [(dec resume-at-idx) idx hash-val countdown]]
                                    (println "Banking: " result-key)
                                    (recur :triple resume-at-idx {:revisit-triples revisit-triples :max-triple-idx max-triple-idx} (conj result-keys result-key)))
                                  (let [new-max-triple-idx? (and trip? (> idx max-triple-idx))
                                        maybe-new-max-triple-idx-value (if new-max-triple-idx? idx max-triple-idx)
                                        new-revisit-triples (if new-max-triple-idx?
                                                              (conj revisit-triples [idx trip?])
                                                              revisit-triples)]
                                    (recur mode
                                           (inc idx)
                                           (-> thousand
                                               (update :countdown dec)
                                               (assoc :revisit-triples new-revisit-triples)
                                               (assoc :max-triple-idx maybe-new-max-triple-idx-value))
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
  (pp/pprint (search-2 seed)))
