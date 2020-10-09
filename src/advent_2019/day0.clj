(ns advent-2019.day0
  (:require [clojure.java.io :as io]
            [au.com.seasoft.general.dev :as dev]))

(defn make-person [first-name surname salary job hobbies age]
  {:first-name (str first-name)
   :surname    (str surname)
   :salary     salary
   :job        (keyword job)
   :hobbies    (map str hobbies)
   :age        age})

;;
;; Read that in as first-name, surname, salary, job, hobbies, age
;; Keep table in an atom
;; Have fn that sorts by the keywords it is given, so [:first-name :salary]
;; Make it able to do ascending/descending
;; What if want to [:first-name :salary hobbies-count]? Pass in comparators?
;;

(def pred-1 (comp #{"Rachael"} :first-name))
(def pred-2 (fn [{:keys [first-name]}]
              ;(dev/log-on first-name (type first-name))
              (= "Mary" first-name)))

(defn some-query [people]
  (let [mary-hobbies (->> people
                          (filter pred-1)
                          first
                          :hobbies)]
    (dev/pp mary-hobbies)))

(def persons-table (atom nil))

;;
;; '+ means ascending, so go from lowest to highest
;; By normal compare function, a is lower than b.
;; (compare "a" "b") => -1, same as (compare 1 2)
;; So making -ive s/make Rachael come before me if doing [[:salary '?] [:first-name '-]]
;; With salary -ive we go from richest.
;; To then pop Mum at the top lets go for higher number of hobbies being most important thing
;;
(def sort-spec [[(fn [{:keys [hobbies]}]
                   (count hobbies)) '-]
                [:salary '-]
                [:age '+]
                [:first-name '+]])

(defn sort-spec->key-fn [sort-spec]
  (fn [person]
    ((apply juxt (mapv first sort-spec)) person)))

;;
;; On each side will expect the number of arguments according to sort-spec. Not that it matters as just process them all.
;; Recursive go thru (loop recur). Each time create a comparator. The args are the other way round if it is a negative.
;; Return result of executing the comparator unless it is 0, in which case recurse again. Always return a number!
;;
(defn sort-spec->comparator [sort-spec]
  (fn [a-args b-args]
    (let [a-b-pairs (map vector a-args b-args)]
      (loop [[pair-ele & rest-pairs] a-b-pairs
             [spec-ele & rest-spec] sort-spec]
        (let [pair-ele (if (= '- (second spec-ele))
                         (reverse pair-ele)
                         pair-ele)
              cf-result (apply compare pair-ele)]
          (if (zero? cf-result)
            (recur rest-pairs rest-spec)
            cf-result))))))

(defn sorted-by
  "Will sort according to a spec that might be created on the UI. As many columns as the user wants, ascending
  or descending. If there are virtual columns (:hobbies-count) then need to substitute in the function for "
  [sort-spec table]
  (->> table
       (sort-by (sort-spec->key-fn sort-spec)
                (sort-spec->comparator sort-spec))
       dev/pp))

(defn fill []
  (let [reader (io/reader (io/resource "day0.edn"))
        people (->> (line-seq reader)
                    (filter seq)
                    (map (fn [s] (read-string (str "[" s "]"))))
                    (map (fn [line] (apply make-person line))))]
    (reset! persons-table people)
    nil))

(comment
  (fill)
  (dev/pp @persons-table)
  (sorted-by sort-spec @persons-table))
