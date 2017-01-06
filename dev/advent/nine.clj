(ns advent.nine
  (:require [utils :as u]
            [clojure.string :as s])
  (:import (java.io StringReader BufferedReader)))

(defn make-repeating-1 [brackets-spec]
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
    (fn [{:keys [mode current bracket-contents take-outs index normals] :as acc} ele]
      (cond
        (and (not= \) ele) (= :mode/within-brackets mode))
        (let []
          (assoc acc :current (str current ele)))

        (and (= \) ele) (= :mode/within-brackets mode))
        (let [brackets-spec (str current ele)
              repeating-spec (make-repeating-1 brackets-spec)]
          (assoc acc :mode :mode/takeout :bracket-contents repeating-spec :current nil))

        (and (= \( ele) (= :mode/normal mode))
        (let []
          (assoc acc :mode :mode/within-brackets
                     :index (inc index)
                     :normals (conj normals {:index index :normal current})
                     :current "("))

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

        ;; Was okay to crash here to get past part one
        (= :mode/normal mode)
        (let []
          ;(assert false (str "Unknown ele: " ele ", mode: " mode ", acc:\n" (:take-outs acc)))
          (assoc acc :current (str current ele))
          )

        :default
        (assert false (str "Unknown ele: " ele ", mode: " mode))
        ))
    {:index 0
     :mode :mode/normal
     :take-outs []
     :normals []
     :current nil}
    in))

(defn first-part-correct []
  (let [input (slurp "./advent/nine.txt")
        raw-series (first (line-seq (BufferedReader. (StringReader. input))))
        _ (println raw-series)
        res (map (fn [take-out]
                   (let [{:keys [counting-out repeat-times]} (:bracket-contents take-out)]
                     (* counting-out repeat-times))) (:take-outs (first-parse raw-series)))]
    (apply + res)))

;;
;; At highest level of recursion there are 2, a parent and a leaf
;;
(def test-input-1 "(25x11)(1x1)J(12x14)CNZKOSNAJVYL(16x3)QADCLDFUVLLZZYKX")
(def test-input-2 "(16x3)QADCLDFUVLLZZYKX")
(def test-input-3 "(3x3)XYZ")
(def test-input-4 "X(8x2)(3x3)ABCY")

;; 241920, and that's what get
(def test-input-5 "(27x12)(20x12)(13x14)(7x10)(1x12)A")

;; S/be only 445, and that's what get
(def test-input-6 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")

;; S/be 25, and that's what get
(def test-input-7 "(3x3)ABC(2x3)XY(5x2)PQRST")

;; Is 9 by (3x2)TWO(5x7)SEVEN
;; So 9 x (35 + 6) = 369 is correct
(def test-input-8 "(18x9)(3x2)TWO(5x7)SEVEN")

(defn make-repeating-2 [brackets-spec]
  (if (= 0 (s/index-of brackets-spec "("))
    (let [counting-out (u/string->int (u/between "(" "x" brackets-spec))
          repeat-times (u/string->int (u/between "x" ")" brackets-spec))
          close-br (inc (s/index-of brackets-spec ")"))]
      {:counting-out counting-out :repeat-times repeat-times :left-over (apply str (drop close-br brackets-spec))})
    (assert false "Don't call unless starts with a bracket")))

(declare decompressed-length)

;;
;; Want to return vector of string, where each starts with a '(' or character. Only do all at this level.
;;
(defn partition-into-ast [in]
  (let [
        ;_ (println in)
        classified (first-parse in)
        ;_ (println "\nclassified:" (u/pp-str classified 200))
        normal-lengths (map decompressed-length (filter identity (map :normal (:normals classified))))
        take-outs (:take-outs classified)
        ]
    [normal-lengths take-outs]))

(defn decompressed-length [in]
  (let [open-bracket-at (s/index-of in "(")
        ;_ (println "IN:" in)
        ]
    (condp = open-bracket-at
      nil (count in)
      0 (let [{:keys [counting-out repeat-times left-over]} (make-repeating-2 in)
              left-br-indexes (u/indexes-of left-over "(")
              further-brackets-count (count left-br-indexes)
              ;_ (println "further-brackets-count: " further-brackets-count)
              ]
          (case further-brackets-count
            0 (let [res (* counting-out repeat-times)
                    _ (assert (= counting-out (count left-over)) (str "counting-out: " counting-out ", left-over: " left-over))
                    ;unaccounted-for (apply str (drop counting-out left-over))
                    ;_ (println (str "Not Accounting for " unaccounted-for ", " (count unaccounted-for)))
                    ]
                (+ res #_(decompressed-length unaccounted-for)))
            1 (let [close-br (inc (s/index-of in ")"))
                    after-close (apply str (drop close-br in))
                    shorter (apply str (take counting-out after-close))
                    left-over (apply str (drop counting-out after-close))
                    ;_ (println "after-close:" after-close)
                    ;_ (println "shorter:" shorter)
                    ;_ (println "left-over:" left-over)
                    ;_ (println "repeat-times:" repeat-times)
                    ]
                (+ (* repeat-times (decompressed-length shorter)) (decompressed-length left-over)))
            (let [[normal-lengths take-outs] (partition-into-ast in)
                  takeout-lengths (map (fn [n s] (* n (decompressed-length s))) (map #(-> % :bracket-contents :repeat-times) take-outs) (map :takeout take-outs))
                  ;_ (println "normal lengths: <" normal-lengths ">")
                  ;_ (println "takeout lengths: <" takeout-lengths ">")
                  ]
              (+ (apply + normal-lengths) (apply + takeout-lengths))
              )
            )
          )
      (let [before-bracket (take open-bracket-at in)]
        (+ (count before-bracket) (decompressed-length (apply str (drop open-bracket-at in)))))
      )))

(defn second-part-correct []
  (let [raw-input (slurp "./advent/nine.txt")
        in (first (line-seq (BufferedReader. (StringReader. raw-input))))
        _ (println in)
        res (time (decompressed-length in))]
    res))

(defn x []
  (decompressed-length test-input-4))

(defn test-various []
  (assert (= 369 (decompressed-length test-input-8)))
  (assert (= 25 (decompressed-length test-input-7)))
  (assert (= 445 (decompressed-length test-input-6)))
  (assert (= 241920 (decompressed-length test-input-5)))
  (assert (= 20 (decompressed-length test-input-4)))
  (assert (= 9 (decompressed-length test-input-3)))
  )
