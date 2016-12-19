(ns advent.nineteen
  (:require [utils :as u]))

(defn make-eleves [num]
  (for [i (range num)]
    {:elf-num (inc i) :num-presents 1}))

(defn remove-elf [elves elf]
  (let [before-count (count elves)
        res (remove #{elf} elves)
        _ (assert (> before-count (count res)) (str "Did not remove " elf " from " elves))
        ]
    res))

(defn lupdate [list n function]
  (let [[head & tail] (drop n list)]
    (concat (take n list)
            (cons (function head) tail))))

(defn present-turns-slow [eleves]
  (loop [eleves eleves
         elf-num 1
         times 0]
    (let [eleves-count (count eleves)]
      (if (> eleves-count 1)
        (let [
              display? (= 0 (rem eleves-count 100000))
              _ (when display? (println "Going..."))
              ;_ (println "elves: " eleves)
              ;_ (println "elf-num: " elf-num)
              this-elf (some #(when (= (:elf-num %) elf-num) %) eleves)
              _ (assert this-elf (str "No elf found at <" elf-num "> from " eleves))
              next-elf (first (drop-while (fn [elf]
                                            (<= (:elf-num elf) elf-num)) eleves))
              next-elf (or next-elf (first eleves))
              _ (assert next-elf (str "No elf found after <" elf-num "> from " eleves))
              ]
          (if (zero? (:num-presents this-elf))
            (recur (remove-elf eleves this-elf) (:elf-num next-elf) (inc times))
            (let [current-elf-idx (u/index-of eleves this-elf)
                  _ (assert current-elf-idx (str "No idx found for <" this-elf "> from " (count eleves)))
                  next-elf-idx (u/index-of eleves next-elf)
                  _ (assert next-elf-idx (str "No idx found for <" next-elf "> from " (count eleves)))
                  ;_ (println "to update these:" current-elf-idx next-elf-idx "have" (count eleves))
                  num-presents-to-transfer (:num-presents (nth eleves next-elf-idx))
                  presents-exchanged-elves (-> eleves
                                               (lupdate current-elf-idx (fn [elf]
                                                                          (update elf :num-presents #(+ % num-presents-to-transfer))))
                                               (lupdate next-elf-idx (fn [elf]
                                                                       (update elf :num-presents #(- % num-presents-to-transfer)))))]
              (recur presents-exchanged-elves (:elf-num next-elf) (inc times))
              )))
        eleves))))

(defn present-turns [starting-eleves]
  (loop [left-vector []
         right-list starting-eleves
         times 0]
    (let [
          [thief & tail] right-list
          ;_ (println thief tail)
          victim (first tail)
          ]
      (if victim
        (let [num-presents-to-take (:num-presents victim)
              _ (assert thief (str "How no thief when victim: " victim ", BAD <" right-list ">"))
              _ (assert (number? num-presents-to-take) (str "strange: " victim))
              happy-thief (update thief :num-presents #(+ % num-presents-to-take))
              ; Don't even need to do this
              ;sad-victim (update victim :num-presents #(- % num-presents-to-take))
              ]
          (recur (conj left-vector happy-thief) (drop 2 right-list) (inc times)))
        (let [
              ;_ (println "No victim:" left-vector right-list)
              [one two] left-vector]
          (if two
            (let [
                  ;_ (println "Maybe thief, but on left: " (seq left-vector))
                  ]
              (if (nil? thief)
                (recur [] (seq left-vector) (inc times))
                (recur [] (cons thief (seq left-vector)) (inc times))))
            (let [
                  ;_ (assert (= 1 (count left-vector)))
                  ]
              (if (empty? right-list)
                one
                (recur [] (concat right-list left-vector) (inc times))))))))
    ))

(def num-eleves 3017957)

(defn x []
  (let [elves (make-eleves num-eleves)]
    (present-turns elves)))
