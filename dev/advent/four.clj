(ns advent.four
  (:require [clojure.string :as s]
            [utils :as u]
            [clojure.java.io :as io]))

(def input-1 "aaaaa-bbb-z-y-x-123[abxyz]")
(def input-2 "qzmt-zixmtkozy-ivhz-343[abxyz]")
(def input-3 "a-b-c-d-e-f-g-h-987[abcde]")

(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def alphabet-size (count alphabet))
(def lookup-alphabet (into {} (map-indexed (fn [idx letter] [letter (inc idx)]) alphabet)))

(defn rotate [rotate-by]
  (fn [letter]
    (let [
          on-by (rem rotate-by alphabet-size)
          letter-at (get lookup-alphabet letter)]
      (if letter-at
        (let [
              ;_ (println "letter-at: " letter-at ", on-by: " on-by " for " letter)
              moved-on (+ on-by letter-at)
              ;_ (println "moved-on: " moved-on)
              in-range (rem moved-on alphabet-size)
              ;_ (println "in-range: " in-range)
              new-letter (nth alphabet (if (zero? in-range) (dec alphabet-size) (dec in-range)))]
          new-letter)
        \space))))

(defn my-decrypt [in]
  (let [
        ;_ (println "decrypt" in)
        before-sector-idx (s/last-index-of in "-")
        letters (take before-sector-idx in)
        sector-id (u/string->int (apply str (take-while #(not= \[ %) (drop (inc before-sector-idx) in))))
        rotate-f (rotate sector-id)
        rotated (map rotate-f letters)]
    {:decrypted (apply str rotated) :sector-id sector-id}))

(defn cf [cmp-a cmp-b]
  (let [size-diff (- (:size cmp-a) (:size cmp-b))
        ;_ (println "size-diff:" size-diff cmp-a cmp-b)
        ]
    (if (not (zero? size-diff))
      (- size-diff)
      (let [x-letter (:letter cmp-a)
            y-letter (:letter cmp-b)
            cf-res (compare x-letter y-letter)
            ;_ (println cf-res "from" x-letter y-letter)
            ]
        cf-res))))

(defn get-sector-id [in]
  (let [before-sector-idx (s/last-index-of in "-")
        sector-id (u/string->int (apply str (take-while #(not= \[ %) (drop (inc before-sector-idx) in))))
        letters (remove #{\-} (take before-sector-idx in))
        grouped (group-by identity letters)
        xs (map (fn [v] {:size (count v) :letter (first v)}) (vals grouped))
        sorted-xs (sort cf xs)
        ;_ (println sorted-xs)
        res (apply str (take 5 (map :letter sorted-xs)))
        chksum (u/between "[" "]" in)
        ;_ (println sector-id)
        ]
    (when (= res chksum)
      sector-id)))

(defn x-first-part []
  (let [
        raw-series (line-seq (io/reader (io/resource "four.txt")))
        series (remove nil? (map get-sector-id raw-series))
        ;_ (get-sector-id (nth raw-series 2))
        ]
    (apply + series)
    ))

(defn x-second-part []
  (let [
        raw-series (line-seq (io/reader (io/resource "four.txt")))
        series (map my-decrypt raw-series)
        possibilities (filter #(s/index-of (:decrypted %) "north") series)
        ]
    possibilities))

(defn x-2 []
  (my-decrypt input-2))

(defn x-3 []
  ((rotate 3) \y))

;;
;; Following Bruce's more concise and easier to read code
;;

(defn parse-room [s]
  (let [parts (s/split s #"-")
        [id chk] (s/split (last parts) #"\[")]
    {:word   (apply concat (butlast parts))
     :chksum (butlast chk)
     :id     (Integer/parseInt id)}))

;;
;; sort-by takes a keyfn - remember `sort-by count`
;; It is all about one element - here one mapentry
;; Here frquencies will be returning [letter freq] mapentries
;; sorting naturally goes from lowest to highest - hence `(- freq)`
;; so most frquent first.
;; `(int \a)` gives ascii number of letter \a => 97.
;; `(int \b)` is 98 - so will be sorting alphabetically secondarily
;;
(defn checksum [word]
  (->> word
       frequencies
       (sort-by (fn [[letter freq]] [(- freq) (int letter)]))
       (map first)
       (take 5)))

(defn real-room? [{:keys [word chksum] :as room}]
  (= (checksum word) chksum))

(comment
  (real-room? (parse-room "aaaaa-bbb-z-y-x-123[abxyz]"))
  (real-room? (parse-room "a-b-c-d-e-f-g-h-987[abcde]"))
  (real-room? (parse-room "not-a-real-room-404[oarel]"))
  (real-room? (parse-room "totally-real-room-200[decoy]"))
  )

;; part 1
#_(->> lines
       (map parse-room)
       (filter real-room?)
       (map :id)
       (reduce +))
;;=> 278221

(defn bruce-part-1 []
  (->> (line-seq (io/reader (io/resource "four.txt")))
       (map parse-room)
       (filter real-room?)
       (map :id)
       (reduce +)))

;;
;; switch to ascii decimal and back again
;; int to get into ascii and char to come back again
;;
(defn shift-letter [n]
  #(-> %
       int
       (- 97)
       (+ n)
       (mod 26)
       (+ 97)
       char))

(defn decrypt [{:keys [word id] :as room}]
  (assoc room :decrypted
              (apply str (map (shift-letter id) word))))

;;
;; He used re-matches. re-find is better as don't need to match whole string, so don't need to know
;; about regular expressions
;;
(defn bruce-part-2 []
  (->> (line-seq (io/reader (io/resource "four.txt")))
       (map parse-room)
       (filter real-room?)
       (map decrypt)
       (filter #(re-find #"north" (:decrypted %)))))
