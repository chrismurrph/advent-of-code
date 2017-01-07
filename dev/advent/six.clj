(ns advent.six
  (:require [utils :as u]
            [clojure.java.io :as io]))

(defn transpose [s]
  (vec (apply mapv str s)))

(def input-1 '({\e 3, \d 2, \r 2, \a 1, \t 2, \s 2, \n 2, \v 2} {\e 2, \r 2, \a 3, \t 2, \s 2, \d 1, \v 2, \n 2} {\d 2, \v 1, \n 2, \a 2, \e 2, \r 2, \t 2, \s 3} {\a 2, \t 3, \d 2, \v 2, \n 2, \r 2, \s 2, \e 1} {\d 2, \e 3, \s 2, \r 2, \t 2, \v 2, \n 1, \a 2} {\n 2, \e 2, \r 3, \d 2, \s 2, \v 2, \a 2, \t 1}))

(def input-2 {\e 3, \d 2, \r 2, \a 1, \t 2, \s 2, \n 2, \v 2})

;;
;; sort-by would have been better than this reduce
;;

(defn get-mini [in]
  (reduce
    (fn [{:keys [mini character]} [k v]]
      (if (< v mini)
        {:mini v :character k}
        {:mini mini :character character}))
    {:mini Integer/MAX_VALUE :character nil}
    in))

(defn msg [in]
  (apply str (map #(:character (get-mini %)) in)))

(defn x []
  (let [
        raw-series (line-seq (io/reader (io/resource "six.txt")))
        series (transpose raw-series)
        freqs (map frequencies series)
        message (msg freqs)
        ]
    message))

#_(defn x-3 []
    (reduce
      (fn [{:keys [maxi character]} [k v]]
        (if (> v maxi)
          {:maxi v :character k}
          {:maxi maxi :character character}))
      {:maxi 0 :character nil}
      input-2))

