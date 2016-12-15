(ns advent.fourteen
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]))

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn decent-hash? [hash]
  hash)

;;
;; Have to go further than need to b/c they will be processed in different order. They will be initially lined
;; up in the order of 'discovered in the next 1000'.  For example idx 39 is discovered at 816 and 92 is
;; discovered at 200. So if you only went to the first only idx 92 would appear. We want 64 but must go further
;; to get it.
;;
(def only-go-to 100)
(def only-interested-in-idx 22728)
(def only-interested-in-nth (dec 64))

(defn printer [idx]
  (fn [txt]
    (when (= idx only-interested-in-idx)
      (println txt))))

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
        ;_ (println "existing-triples: " existing-triples)
        ;_ (println "triple-adding: " triple-adding)
        ;_ (println "inhibitors: " inhibitors)
        ;_ (println "")
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
         triples []]
    (let [pr (printer idx)
          hash-val (md5 (str seed idx))
          _ (pr (str "-------------->hash: " hash-val))
          _ (assert (decent-hash? hash-val) (str "Did not manage to get a hash: " seed ", " idx))
          triple? (triple-from-hash hash-val)]
      (if (>= (count result-keys) only-go-to)
        result-keys
        (let [
              ;_ (pr (str "triples: " (mapv :index triples)))
              triples-with-old-removed (vec (remove-old-triples idx triples))
              ;_ (pr (str "triples-with-old-removed: " (mapv :index triples-with-old-removed)))
              debug-reduce? (some #(= only-interested-in-idx %) (mapv :index triples-with-old-removed))
              found-five-timer (reduce
                                 (fn [{:keys [res done?] :as accum} {:keys [five-times-fn index hash triple] :as elem}]
                                   (if (not done?)
                                     (let [res? (five-times-fn hash-val)]
                                       (if res?
                                         (let [new-five-timer {:orig-hash       hash
                                                               :triple          triple
                                                               :five-times-hash hash-val
                                                               :orig-index      index
                                                               :found-index     idx}
                                               _ (when debug-reduce?
                                                   (println (str "new-acc: " new-five-timer)))
                                               ]
                                           {:res new-five-timer :done? true})
                                         accum))
                                     (do
                                       (when debug-reduce?
                                         ;(println (str "Missing: " elem))
                                         )
                                       accum)))
                                 {:res nil :done? false}
                                 triples-with-old-removed)
              _ (when (and (:done? found-five-timer)
                           (= (:orig-index found-five-timer) only-interested-in-idx))
                  (println "--------> found-five-timer:" (:res found-five-timer)))
              remover (triple-remover (-> found-five-timer :res :orig-index))
              updated-triples (vec (remove remover triples-with-old-removed))
              new-triple (when triple?
                           ;(println "triple:" triple? "at" idx)
                           {:triple        triple?
                            :index         idx
                            :hash          hash-val
                            :five-times-fn (five-times? (apply str (repeat 5 (first triple?))))})
              further-updated-triples (if new-triple (add-new-triple new-triple updated-triples) updated-triples)
              _ (pr (last further-updated-triples))
              new-result-keys (if (:res found-five-timer)
                                (conj result-keys (:res found-five-timer))
                                result-keys)
              ]
          (recur (inc idx) new-result-keys further-updated-triples)
          )))))

(def seed "abc")

(defn x []
  (let [sorted-results (sort-by :orig-index (search-2 seed))
        res (nth sorted-results only-interested-in-nth)
        interested (filter #(= only-interested-in-idx (:orig-index %)) sorted-results)
        ]
    (pp/pprint interested)
    (pp/pprint (filter #(= 22804 (:found-index %)) sorted-results))))

(comment
  (defn x-1 []
    (let [hash (md5 "abc18")
          trip (triple-from-hash hash)
          to-search (apply str (repeat 5 (first trip)))
          five-times-sample "00888"
          _ (println to-search)]
      ((five-times? to-search) five-times-sample))))
