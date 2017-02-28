(ns hackerrank.minimise
  (:require [utils :as u]))

(def safe-neighbors
  "The characters that indicate neighboring spaces are safe to remove."
  (set "(){}[]\" \t\n"))

(def minimizer-output-path "./minimized-code.clj")

(defn can-remove-cur-char?
  "Returns whether or not a character is safe to remove.
   A character is considered unsafe to remove if it doesn't change the meaning of the program.
   Expects that if prev-char or next-char aren't available, nil will be passed."
  [prev-char cur-char next-char]
  (cond
    (not (Character/isWhitespace ^Character cur-char))
    false

    (or (nil? prev-char) (nil? next-char))
    true

    :else
    (or (safe-neighbors prev-char) (safe-neighbors next-char))))

(defn safe-remove-whitespace
  "Removes any whitespace characters that don't effect the meaning of the program."
  [code-str]
  (loop [[chr & rest-str] code-str
         acc ""]
    (if-not chr
      acc
      (let [prev-chr (if (empty? acc) nil (last acc))
            next-chr (if rest-str (first rest-str) nil)
            can-remove? (can-remove-cur-char? prev-chr chr next-chr)
            replacement-chr (if (Character/isWhitespace ^Character chr)
                              \space chr)]
        (recur rest-str
               (if can-remove? acc (str acc replacement-chr)))))))

(defn minimize-code-1*
  "Removes unnecessary whitespace and outputs it to minimizer-output-path, then returns the minimized code."
  [code-str]
  (let [minimized-code (safe-remove-whitespace code-str)]
    (spit minimizer-output-path minimized-code)
    minimized-code))

(defn whitesp? [ch]
  (Character/isWhitespace ^Character ch))

(defn del-white [s]
  (reduce #(if (and (whitesp? %2)
                    (whitesp? (last %1)))
             %1
             (str %1 %2)) "" s))

;;
;; Not doing so, but would be possible for the determination to be made with
;; pairs in both orders (i.e. [ab] is fine but [ba] is not)
;;
(defn squeezable? [[a b c]]
  ;(and (safe-neighbors a) (whitesp? b) (safe-neighbors c))
  false
  )

(defn elide [triples]
  (let [squeezabilities (drop-last (cons true (map squeezable? triples)))
        _ (println squeezabilities)
        _ (assert (= (count triples) (count squeezabilities)))
        together (map vector squeezabilities triples)]
    (->> together
         (filter first)
         (map (comp first second)))))

;;
;; If middle is white and they are safe neighbours then we will skip the next one.
;; If always take the first after this skip then the inessential whitespace will have been deleted.
;; To do this get a list of trues and falses and shift one to the right.
;; map vector these together then filter
;;
(defn squeeze-safe [s]
  (assert (string? s) (type s))
  ;(println (str "<" (take-last 2 s) ">"))
  (apply str (apply str (->> s (partition 3 1) (map first))) (take-last 2 s)))

(defn minimize-code-2*
  [code-str]
  (->> code-str
       del-white
       squeeze-safe
       ))

(defmacro minimize-code
  "Removes unneccesary whitespace and outputs it to minimizer-output-path, then returns the minimized code."
  [& body]
  (minimize-code-2* (apply str body)))

(defn x-1 []
  (minimize-code-2* "(fn [a]    (+ a   1))"))
