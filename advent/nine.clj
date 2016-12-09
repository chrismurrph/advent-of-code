(ns advent.nine
  (:require [utils :as u])
  (:import (java.io StringReader BufferedReader)))

(defn make-repeating [brackets-spec]
  (let [counting-out (u/string->int (u/between "(" "x" brackets-spec))
        repeat-times (u/string->int (u/between "x" ")" brackets-spec))]
    {:counting-out counting-out :repeat-times repeat-times}))

;;
;; :current is disposably fed into depending on the mode:
;; :mode/normal -> normal characters keep getting placed in
;; :mode/within-brackets -> specification is being read in (later delivered to within a breakout under :bracket-contents)
;; :mode/takeout -> specific number of characters following say '(4x14)'
;;
(defn first-parse [in]
  (reduce
    (fn [{:keys [mode current bracket-contents take-outs index] :as acc} ele]
      (cond
        (and (not= \) ele) (= :mode/within-brackets mode))
        (let []
          (assoc acc :current (str current ele)))

        (and (= \) ele) (= :mode/within-brackets mode))
        (let [brackets-spec (str current ele)
              repeating-spec (make-repeating brackets-spec)]
          (assoc acc :mode :mode/takeout :bracket-contents repeating-spec :current nil))

        (and (= \( ele) (= :mode/normal mode))
        (let []
          (assoc acc :mode :mode/within-brackets :current "("))

        (and (= \) ele) (= :mode/within-brackets mode))
        (let []
          (assoc acc :mode :mode/takeout))

        (= :mode/takeout mode)
        (let [counting-out (:counting-out bracket-contents)
              done? (= (count current) (dec counting-out))]
          (if done?
            (let []
              ;(assert false (str "done: " current))
              (assoc acc :mode :mode/normal
                         :current nil
                         :bracket-contents nil
                         :index (inc index)
                         :take-outs (conj take-outs {:index index :takeout (str current ele) :bracket-contents bracket-contents})))
            (assoc acc :current (str current ele))))

        (= \) ele)
        (assert false (str "Not expected closing bracket"))

        (= :mode/normal mode)
        (let []
          (assert false (str "Unknown ele: " ele ", mode: " mode)))

        :default
        (assert false (str "Unknown ele: " ele ", mode: " mode))
        ))
    {:index 0
     :mode :mode/normal
     :take-outs {}
     :normals {}
     :current nil}
    in))

(defn x []
  (let [input (slurp "./advent/nine.txt")
        raw-series (first (line-seq (BufferedReader. (StringReader. input))))
        _ (println raw-series)
        res (first-parse raw-series)]
    (dissoc res :take-outs)))
