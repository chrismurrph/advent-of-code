(ns utils
  (:require [clojure.string :as s]
            [dev :as dev]))

(defn str->number [x]
  (when-let [num (re-matches #"-?\d+\.?\d*" x)]
    (try
      (bigdec num)
      (catch Exception _
        nil))))

;;
;; Won't work in assert because of namespace issues, so copy into your own file until
;; I become better at macros (or ask on SO)
;;
(def counter (atom 0))
(defn true-until [counted]
  (when (not= counted @counter)
    (do
      (swap! counter inc)
      true)))

;;
;; Returns the indexes that satisfy the predicate
;; Note that positions (same function) uses keep-indexed - three functions into one!
;;
(defn -indexes-by [f coll]
  (->> coll
       (map-indexed vector)
       (filter (comp f second))
       (map first)))

;;
;; Great way of ensuring don't have intermediate collections
;; sequence is 'lazy transduced'
;; into [] is 'eager transduced'
;;
(defn indexes-by-fancy-1 [f coll]
  (sequence
    (comp
      (map-indexed vector)
      (filter (comp f second))
      (map first))
    coll))

(defn indexes-by-fancy-2 [f coll]
  (comment "Bet we can do it with a transducer too"))

(def indexes-by -indexes-by)

(defn get-now [] (.getTime (java.util.Date.)))

(def startup-time (get-now))

;(println (str "startup-time is " startup-time))

(defn elapsed []
  (- (get-now) startup-time))

(defn strip-edges [s]
  (apply str (take (- (count s) 2) (drop 1 s))))

(defn left-bank [left coll]
  (vec (concat left (drop (count left) coll))))

(defn right-bank [right coll]
  (let [diff (- (count coll) (count right))]
    (vec (concat (take diff coll) right))))

(defn after [begin-marker s]
  (apply str (drop (count begin-marker) (drop-while #(not= % (first begin-marker)) s))))

(defn before [end-marker s]
  (apply str (take-while #(not= % (first end-marker)) s)))

(defn between [begin-marker end-marker s]
  (let [after-fn #(after begin-marker %)
        before-fn #(before end-marker %)
        blank->nil-fn #(if (= % "") nil %)]
    (-> s after-fn before-fn blank->nil-fn)))

(defn string->int [s]
  (assert s)
  (assert (or (string? s) (char? s)) (str "Wrong type: <" (type s) ">, <" s ">"))
  (try
    (Long/parseLong (str s))
    (catch NumberFormatException _
      (assert false (str "Cannot parse as an int: <" s ">"))
      nil)))

(def to-int #(Integer/parseInt %))
(def to-ints (partial map to-int))

;(defn int-between [s1 s2 s]
;  (string->int (subs s (count s1) (count s2))))

(defn has-key? [m kw]
  (boolean (and (map? m) (some #{kw} (keys m)))))

(defn not-blank? [x]
  (or (boolean? x) (and x (not= x ""))))

;;
;; Returns the index positions of those that satisfy the pred(icate)
;;
(defn positions
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))

(defn drop-nth [n coll]
  (keep-indexed #(if (not= %1 n) %2) coll))

(defn index-of [coll desired]
  (first (keep-indexed (fn [idx val] (when (= val desired) idx)) coll)))

(defn first-no-more [seq]
  (assert (= nil (second seq)) (str "Only supposed to be one. However:\nFIRST:\n" (first seq) "\nSECOND:\n" (second seq)))
  (first seq))

(def third #(nth % 2))
(def fourth #(nth % 3))
(def fifth #(nth % 4))

;
; s and value never change but from-index is recursed, so can use loop recur on just that
;
(defn- whole-word-index-of [^CharSequence s value ^long from-index after?]
  (let [whitespace? (fn [^Character c]
                      (or (= c \newline) (= c \return) (= c \space) (= c \tab)))
        _ (assert value)
        _ (assert s (str "Can't look for <" value "> in nil"))
        _ (assert from-index)
        res (s/index-of s value from-index)]

    (if (nil? res)
      nil
      (let [just-before (first (take 1 (drop (dec res) s)))
            whitespace-before? (whitespace? just-before)
            end-res (+ res (count value))
            just-after (first (take 1 (drop end-res s)))
            whitespace-after? (whitespace? just-after)
            whole-word? (and whitespace-before? whitespace-after?)]

        (if whole-word?
          (if after? (+ res (count value)) res)
          (whole-word-index-of s value end-res after?))))))

(defn indexes-of-whole-word [s value after?]
  (loop [acc [] idx 0]
    (let [res (whole-word-index-of s value idx after?)]
      (if (nil? res)
        acc
        (recur (conj acc res) (inc res))))))

(defn indexes-of [s value]
  (loop [acc [] idx 0]
    (let [res (s/index-of s value idx)]
      (if (nil? res)
        acc
        (recur (conj acc res) (inc res))))))

#_(defn indexes-of [s coll]
  (loop [res [] idx 0]
    (let [found-idx (s/index-of coll s idx)]
      (if found-idx
        (recur (conj res found-idx) (inc found-idx))
        res))))

(defn insert-at [s x n]
  (apply str (concat (take n s) x (drop n s))))

(defn indexes-of-many-whole-words [s values after?]
  (map #(indexes-of-whole-word s %  after?) values))

#_(defn test-whole-word []
    (let [s prod-input
          want "TAG"
          res (u/whole-word-index-of s want 0)]
      res))

(defn rm-punctuation [in-str]
  (apply str (remove #{\. \? \!} in-str)))

(defn abs [val]
  (if (neg? val)
    (* -1 val)
    val))

(defn whole-number? [n]
  (zero? (rem n 1)))

#_(defn whole-number? [n]
  (= 0.0 (rem n 1)))

(defn sqrt [n]
  (if (> n 0)
    (Math/sqrt n)
    0))

(defn exp [x pow-of]
  (Math/pow x pow-of))

(defn root [x root-of]
  (Math/pow x (/ root-of)))

(defn round [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn divide [num div]
  (let [res (/ num div)]
        ;_ (assert (= res (int res)) (str "Got back fraction: " res))

    (round 0 res)))

(defn factorial [n]
  (reduce * (range 1N (inc n))))

(defn intersection
  [s1 s2]
  (if (< (count s2) (count s1))
    (recur s2 s1)
    (reduce (fn [result item]
              (if (contains? s2 item)
                result
                (disj result item)))
            s1 s1)))

(defn divisors [n]
  (into #{} (mapcat #(when (zero? (rem n %)) [% (/ n %)])
                    (range 1 (Math/ceil (Math/sqrt n))))))

(defn gcd [a b]
  (fn [a b]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn lcm [& numbers]
  (let [gcd-fn (fn [a b] (if (zero? b)
                          a
                          (recur b (mod a b))))
        lcm-inner (fn [num1 num2]
                   (let [multiplied (* num1 num2)
                         gcd (gcd-fn num1 num2)
                         res (/ multiplied gcd)]
                     res))
        [head & tail] numbers]
    (if (nil? tail)
      head
      (lcm-inner head (apply lcm tail)))))

;;
;; from-world and to-world are maps of type {:min _ :max _}
;; These max and min are inclusive, so the exact middle when :min 0 and :max 10 is 5
;; Note that we need to do precision-scaling at the end, as there needs to be an exact
;; pixel location where to put circle on the graph
;;
(defn scale [from-world to-world from-val]
  (let [min-from (:min from-world)
        max-from (:max from-world)
        min-to (:min to-world)
        max-to (:max to-world)
        from-diff (- max-from min-from)
        to-diff (- max-to min-to)
        from-proportion (/ (- from-val min-from) from-diff)
        res (* to-diff from-proportion)
        rounded-res (int (Math/ceil res))]
        ;_ (println "FROM VAL:" from-val " | RES:" rounded-res " | " res " | F:" from-world " | T:" to-world)

    rounded-res))

(defn distance [precision [x1 y1] [x2 y2]]
  (let [x-delta-squared (exp (- x2 x1) 2)
        y-delta-squared (exp (- y2 y1) 2)
        sum-of-differences (+ x-delta-squared y-delta-squared)
        now-squared (sqrt sum-of-differences)]
    (round precision now-squared)))

(defn perfect-square? [n]
  (-> n sqrt whole-number?))

(defn gen-primes [n]
  (letfn [(sieve [s]
            (cons (first s)
                  (lazy-seq (sieve (filter #(not= 0 (mod % (first s)))
                                           (rest s))))))]
    (take n (sieve (iterate inc 2)))))

(defn comma-str->ints
  [string]
  (map #(Long/parseLong %)
       (s/split string #",")))

(defn space-str->ints
  [string]
  (map #(Long/parseLong %)
       (s/split string #" ")))

(defn many-line-reader [lines item-fn no-overall-header]
  (let [seed {:expecting (if no-overall-header :case-header :overall-header)
              :results   []}
        output (reduce
                 (fn [acc ele]
                   (let [nxt-acc (case (:expecting acc)
                                   :overall-header
                                   (assoc acc :expecting :case-header)
                                   :case-header
                                   (-> acc
                                       (assoc :current-header ele)
                                       (assoc :expecting :item))
                                   :item
                                   (let [res (item-fn ele (:current-header acc))]
                                     (-> acc
                                         (update :results conj res)
                                         (assoc :expecting :case-header)))
                                   acc)]
                     nxt-acc))
                 seed
                 lines)
        res (:results output)]
    res))

(defn string->int? [s]
  (assert (string? s) (str "Wrong type (not string), where value is: " s ", type is " (type s)))
  (try
    (Long/parseLong s)
    (catch NumberFormatException _
      nil)))

(defn char->int? [c]
  (assert (char? c) (str "Wrong type (not char), where value is: " c ", type is " (type c)))
  (try
    (Long/parseLong (str c))
    (catch NumberFormatException _
      nil)))

;;
;; Naive function that does not respect the sign bit
;; Where ever this is used but not from convert is a danger sign.
;;
(defn left-pad [xs pad-ele max-sz]
  (let [
        diff-count (- max-sz (count xs))]
        ;_ (assert (or (zero? diff-count) (pos? diff-count)) (str "Max size is " max-sz ", yet already have " (count xs)))

    (if (> (count xs) max-sz)
      xs
      (concat (take diff-count (repeat pad-ele)) xs))))

(defn left-pad-integer [int pad-ele max-sz]
  (let [_ (assert (number? int))
        as-str (str int)
        diff-count (- max-sz (count as-str))
        _ (assert (or (zero? diff-count) (pos? diff-count)) (str "Max size is " max-sz ", yet already have " (count as-str)))
        padded (apply str (concat (repeat diff-count pad-ele) as-str))]

    padded))

(defn sleep [n]
  (Thread/sleep n))

(defmacro assrt
  "Useful to use (rather than official version that this is o/wise a copy of) when don't want intermingling of
  the stack trace produced here with trace output that want to come before"
  {:added "1.0"}
  ([x]
   (when *assert*
     `(when-not ~x
        (sleep 200)
        (throw (new AssertionError (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
   (when *assert*
     `(when-not ~x
        (sleep 200)
        (throw (new AssertionError (str "Assert failed: " ~message "\n" (pr-str '~x))))))))

(defn ends-with? [s ending]
  (= (dec (count s)) (s/last-index-of s ending)))

;(defn fibonacci-seq [size]
;  (loop [acc [1N 1N]
;         ele-at 2N]
;    (if (= ele-at size)
;      acc
;      (let [prior-val (nth acc (dec ele-at))
;            prior-prior-val (nth acc (- ele-at 2N))
;            next-val (+' prior-val prior-prior-val)]
;        (recur (conj acc next-val) (inc ele-at))))))

(defn fibonacci-seq [limit] (mod (nth (map first
                                           (iterate
                                             (fn fib-step [[a b]] [b (+ a b)]) [0N 1])) limit)
                                 100000007))

;(defn- bubble-max-key [k coll]
;  "Move a maximal element of coll according to fn k (which returns a number)
;   to the front of coll."
;  (let [max (apply max-key k coll)]
;    (cons max (remove #(identical? max %) coll))))

;(defn factorial [x]
;  (loop [n x f 1N]
;    (if (= n 1)
;      f
;      (recur (dec n) (* f n)))))

;;
;; Each combination returned is in a specific order and you don't get other
;; combinations of that order 'mixed up'. Once a position for an element is
;; found it is 'not replaced'. If the population was pre-sorted, then that order
;; will be kept amongst all the resultant combinations.
;;
(defn combinations [population sz]
  (cond
    (= sz 0) '(())
    (empty? population) '()
    :else (concat (mapv #(cons (first population) %) (combinations (rest population) (dec sz)))
                  (combinations (rest population) sz))))

;; Doesn't work for strings
#_(defn permutations [s]
  (lazy-seq
    (if (seq (rest s))
      (apply concat (for [x s]
                      (map #(cons x %) (permutations (remove #{x} s)))))
      [s])))

(defn transpose
  "Transposes the given nested sequence into nested vectors, as
  in matrix transposition.  E.g., (transpose [[1 2 3] [4 5 6]])
  would return [[1 4] [2 5] [3 6]]."
  [s]
  (vec (apply mapv vector s)))

(defn shift
  [row n]
  (let [n (- (count row) n)
        [first-part second-part] (split-at n row)]
    (vec (concat second-part first-part))))

(comment
  (defn digits->number [digits]
    (reduce (fn [a b] ('+ ('* a 10) b)) 0 digits)))

(defn digits [n]
  (loop [result (list), n n]
    (if (pos? n)
      (recur (conj result (rem n 10))
             (quot n 10))
      result)))

(comment
  (defn gcd [a b]
    (let [greatest (max a b)
          least (if (= greatest a) b a)
          end-point (min (Math/floor (/ greatest 2)) least)]
      (loop [res 1
             where-at 2]
        (if (<= where-at end-point)
          (let [next-up (inc where-at)]
            (if (and (= 0 (mod greatest where-at)) (= 0 (mod least where-at)))
              (recur where-at next-up)
              (recur res next-up)))
          res)))))

(comment
  (defn gcd [a b]
    (->> (map (fn [x]
                (filter #(zero? (mod x %)) (range 1 (inc x))))
              [a b])
         (map set)
         (apply clojure.set/intersection)
         (apply max))))

;(def stop-at 15)
(def heavy? false)
(defn my-pr-str [labs]
  (if heavy?
    (dev/pp-str labs 100)
    (str (count labs))))

(defn breath-first-search [starting-lab generate-possible-moves destination-state?]
  (loop [already-tested #{starting-lab}
         last-round #{starting-lab}
         total-visited 0
         times 1]
    (let [
          ;where-at (remove already-tested last-round)
          newly-generated (mapcat generate-possible-moves last-round)
          ;_ (println (str "Generated " (my-pr-str newly-generated) " from " (my-pr-str last-round) " at " times))
          got-there? (first (filter destination-state? newly-generated))]
      ;(println (str "Newly generated: " (count newly-generated)))
      (if got-there?
        (let []
          ;(println (str "Got there with: <" got-there? ">"))
          times)
        (let [now-tested (into already-tested newly-generated)]
          (recur now-tested (into #{} (remove already-tested newly-generated)) (+ total-visited (count last-round)) (inc times)))))))
