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

(defn triple-from-hash [hash]
  (first (drop-while #(< (count %) 3) (partition-by identity hash))))

(defn- -md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(def md5 (memoize -md5))

(defn add-new-triple [triple triples]
  (let [existing-triples (into #{} (map :triple triples))
        triple-adding (:triple triple)
        _ (assert triple-adding)
        inhibitors (existing-triples triple-adding)
        _ (println "existing-triples: " existing-triples)
        _ (println "triple-adding: " triple-adding)
        _ (println "inhibitors: " inhibitors)
        _ (println "")
        new-one-added (if triple
                        (conj triples triple)
                        triples)]
    new-one-added))

(defn remove-old-triples [idx triples]
  (let [
        old-removed (remove (fn [{:keys [index]}]
                              (let [age (- idx index)]
                                (>= age 1000))) triples)]
    old-removed))

(defn triple-remover [five-timers-orig-index]
  (fn [{:keys [index]}]
    (= five-timers-orig-index index)))

(defn search-2 [seed]
  (loop [idx 0
         result-keys []
         triples {}]
    (let [hash-val (md5 (str seed idx))
          _ (when (= idx 816) (println "-------------->hash" hash-val))
          _ (assert (decent-hash? hash-val) (str "Did not manage to get a hash: " seed ", " idx))
          triple? (triple-from-hash hash-val)]
      (if (>= (count result-keys) 3)
        result-keys
        (let [
              triples-with-old-removed (remove-old-triples idx triples)
              ;_ (println updated-triples)
              found-five-timer (reduce
                                 (fn [{:keys [res done?] :as accum} {:keys [five-times-fn index hash triple]}]
                                   (if (not done?)
                                     (let [res? (five-times-fn hash-val)]
                                       (if res?
                                         (let [new-five-timer {:orig-hash       hash
                                                               :triple          triple
                                                               :five-times-hash hash-val
                                                               :orig-index      index
                                                               :found-index     idx}
                                               ;_ (println "new-acc: " new-acc)
                                               ]
                                           {:res new-five-timer :done? true})
                                         accum))
                                     accum))
                                 {:res nil :done? false}
                                 triples-with-old-removed)
              ;_ (println found-five-timers)
              remover (triple-remover (-> found-five-timer :res :orig-index))
              updated-triples (remove remover triples-with-old-removed)
              new-triple (when triple?
                           (println "triple:" triple? "at" idx)
                           {:triple        triple?
                            :index         idx
                            :hash          hash-val
                            :five-times-fn (five-times? (apply str (repeat 5 (first triple?))))})
              further-updated-triples (if new-triple (add-new-triple new-triple updated-triples) updated-triples)
              ;_ (println re-updated-triples)
              new-result-keys (if (:res found-five-timer)
                                (conj result-keys (:res found-five-timer))
                                result-keys)
              ]
          (recur (inc idx) new-result-keys further-updated-triples)
          )))))

(def seed "abc")

(defn x-1 []
  (let [hash (md5 "abc18")
        trip (triple-from-hash hash)
        to-search (apply str (repeat 5 (first trip)))
        five-times-sample "00888"
        _ (println to-search)]
    ((five-times? to-search) five-times-sample)))

(defn x []
  (pp/pprint (search-2 seed)))
