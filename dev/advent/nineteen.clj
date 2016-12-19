(ns advent.nineteen
  (:require [utils :as u]))

(defn make-eleves [num]
  (for [i (range num)]
    {:elf-num (inc i) :num-presents 1}))

(defn remove-elf [elves elf]
  (let [before-count (count elves)
        res (vec (remove #{elf} elves))
        _ (assert (> before-count (count res)) (str "Did not remove " elf " from " elves))
        ]
    res))

(defn present-turns [eleves elf-num]
  (loop [eleves eleves
         elf-num elf-num
         times 0]
    (if (> (count eleves) 1)
      (let [
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
                                             (update current-elf-idx (fn [elf]
                                                                       (update elf :num-presents #(+ % num-presents-to-transfer))))
                                             (update next-elf-idx (fn [elf]
                                                                    (update elf :num-presents #(- % num-presents-to-transfer)))))]
            (recur presents-exchanged-elves (:elf-num next-elf) (inc times))
            )))
      eleves)))

(def starting-elf 1)
(def num-eleves 3017957)
(defn x []
  (let [elves #_[{:elf-num 1, :num-presents 2} {:elf-num 3, :num-presents 2} {:elf-num 5, :num-presents 1}] (vec (make-eleves num-eleves))]
    (present-turns elves starting-elf)))
