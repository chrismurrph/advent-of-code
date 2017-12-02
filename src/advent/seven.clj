(ns advent.seven
  (:require [clojure.java.io :as io])
  (:import (java.io StringReader BufferedReader)))

(def input-1 "ioxxoj[asdfgh]zxcvbn")
(def input-2 "ioxxooj[asaba]zxcvbn")
(def input-3 "zazbz[bzb]cdb")

;; {:in-brackets? in-brackets :res res :brackets brackets :current-bracket current-bracket}

(defn partition-brackets [in]
  (let [res (reduce
              (fn [{:keys [in-brackets? current-res brackets current-bracket results]} ele]
                (cond
                  (= \[ ele) {:in-brackets? true :current-res nil :brackets brackets :current-bracket nil :results (conj results current-res)}
                  (= \] ele) {:in-brackets? false :current-res nil :brackets (conj brackets current-bracket) :current-bracket nil :results results}
                  in-brackets? {:in-brackets? in-brackets? :res current-res :brackets brackets :current-bracket (str current-bracket ele) :results results}
                  :default {:in-brackets? in-brackets? :current-res (str current-res ele) :brackets brackets :current-bracket current-bracket :results results}))
              {:in-brackets? false :current-res "" :brackets [] :current-bracket "" :results []}
              in)]
    (update res :results conj (:current-res res))))

(defn separate-parts [s]
  (->> s
       (partition-by #{\[ \]})
       (reduce (fn [{:keys [k] :as st} v]
                 (condp = v
                   [\[] (assoc st :k :neg)
                   [\]] (assoc st :k :pos)
                   (update st k conj v)))
               {:k :pos})))

(defn abba? [x]
  (let [[one two three four] x]
    (and (= one four) (= two three) (not= one two))))

(defn aba? [x]
  (let [_ (assert (= 3 (count x)) (str "Need 3: " (seq x)))
        [one two three] x]
    (when (and (= one three) (not= one two))
      x)))

(defn correspondence? [aba bab]
  (assert (aba? aba) (str "Not aba?: " aba))
  (assert (aba? bab) (str "Not aba?: " bab))
  (let [[aba-one aba-two aba-three] aba
        [bab-one bab-two bab-three] bab
        res (and (= aba-one bab-two) (= aba-two bab-one))]
    (when res
      ;(println (str "correspond: " aba ", " bab))
      res)))

;;
;; Does it correspond to one of the many abas closed over?
;;
(defn corresponding-bab? [aba-coll]
  ;(assert (assert (= 3 (count aba-coll)) (str "Need 3: " (seq aba-coll))))
  (fn [x]
    (let [[one two three] x
          format-ok? (and (= one three) (not= one two))]
      (when format-ok?
        (some #(correspondence? % x) aba-coll))))
  )

(defn x-2 []
  ((corresponding-bab? ["xyx" "aba"]) "yxy"))

(defn rolling? [size f?]
  (fn [s]
    (let [all (partition size 1 s)
          ;_ (println all)
          ]
      (some f? all))))

(defn all-rolling? [size f?]
  (fn [s]
    (let [
          ;_ (println "s" s)
          all (partition size 1 s)
          ;_ (println all)
          ]
      (filter identity (map f? all)))))

(defn x-4 []
  ((all-rolling? 3 aba?) "zazbz"))

(defn tls-secure? [in]
  (let [{:keys [results brackets]} (partition-brackets in)
        ;_ (println "results: " results)
        ;_ (println "brackets: " brackets)
        any-bracket-abba? (some (rolling? 4 abba?) brackets)
        ]
    (when (not any-bracket-abba?)
      (some (rolling? 4 abba?) results))
    ))

(defn x-3 []
  (let [
        input (slurp (io/resource "2016/seven.txt"))
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        ;raw-series [input-2]
        secures (map tls-secure? raw-series)
        _ (println "total lines: " (count secures))
        num-secure (count (filter identity secures))]
    num-secure))

;;
;; From outside brackets collect all rolling aba.
;; From inside brackets collect all rolling bab.
;; (ir is supposed to be same function but coming from different place)
;; Construct corresponding-bab? and use inner fn in some over all rolling bab
;;
(defn ssl-secure-1? [in]
  (let [
        ;{:keys [results brackets]} (partition-brackets in)
        {:keys [pos neg]} (separate-parts in)
        ;_ (println "results: " (apply str results))
        ;; It gives us one more but not right answer
        ;;all-together-results (apply str results)
        ;_ (println "brackets: " brackets)
        ;;all-babs (filter seq (map (comp #(apply str %) (all-rolling? 3 aba?)) neg))
        all-babs (map #(apply str %) (mapcat (all-rolling? 3 aba?) neg))
        ;_ (println "all babs: " all-babs (count all-babs))
        all-abas (map #(apply str %) (mapcat (all-rolling? 3 aba?) pos))
        ;all-abas (filter seq (map (comp #(apply str %) (all-rolling? 3 aba?)) pos))
        ;_ (println "all abas: " all-abas (count all-abas))
        f (corresponding-bab? all-abas)
        res (some f all-babs)
        ;_ (println res)
        ]
    res))

(defn ssl? [[a b c :as x]]
  {:pre [(= 3 (count x))]}
  (and (not= a b) (= a c)))

(defn ssl-inv? [[a b c :as x] [a1 b1 c1 :as y]]
  {:pre [(= 3 (count x)) (= 3 (count y)) (aba? x)]}
  (and (= a b1) (= b a1 c1)))

(defn contains-pred? [f s]
  (some f (partition 3 1 s)))

(defn ssl-secure-2? [in]
  (let [{:keys [pos neg]} (separate-parts in)]
    (some identity
          (for [ssl-x (mapcat #(filter aba? (partition 3 1 %)) pos)]
            (some #(contains-pred? (partial ssl-inv? ssl-x) %) neg)))))

(defn x []
  (let [
        input (slurp (io/resource "2016/seven.txt"))
        raw-series (line-seq (BufferedReader. (StringReader. input)))
        ;raw-series [input-2]
        secures (map ssl-secure-1? raw-series)
        _ (println "total lines: " (count secures))
        num-secure (count (filter identity secures))]
    num-secure))
