(ns advent.ten
  (:require [instaparse.core :as insta]
            [utils :as u]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :refer [difference]]))

(def test-input-line "bot 75 gives low to bot 145 and high to bot 95")
(declare test-lines)

;; bot 75 gives low to bot 145 and high to bot 95
(def grammar-1
  (insta/parser
    "<S> = bot-gives | value-goes
     int = #'[0-9]+'
     value-goes = <'value '> int <' goes to bot '> int
     bot = <'bot'>
     output = <'output'>
     <entity> = bot | output
     bot-gives = <'bot '> int <' gives low to '> entity <' '> int <' and high to '> entity <' '> int"))

(defn to-hiccup [s]
  (->> (first (grammar-1 s))
       (insta/transform
         {:int clojure.edn/read-string})))


(defn produce-instructions [hiccup]
  (reduce
    (fn [acc [cmd & tail]]
      (case cmd
        :value-goes (let [[value bot-num] tail]
                      (conj acc {:entity {:type :bot :entity-num bot-num} :value value}))
        :bot-gives acc))
    []
    hiccup))

(defn produce-bots-hash [hiccup]
  (reduce
    (fn [acc [cmd & tail]]
      (case cmd
        :value-goes acc
        :bot-gives (let [[bot-num [low-entity-type] low-entity-value [high-entity-type] high-entity-value] tail]
                     (assoc acc bot-num {:low {:type low-entity-type :entity-num low-entity-value}
                                         :high {:type high-entity-type :entity-num high-entity-value}
                                         :values []}))))
    {}
    hiccup))

(def watch-compare #_#{61 17} #{5 2})
(def do-part-one true)

;;
;; Returns a bots state after an instruction.
;;
(defn give-one [bots]
  ;(println bots)
  (fn [{:keys [from-bot entity value] :as instruction}]
    (let [{:keys [type entity-num]} entity]
      (if (= type :bot)
        (let [bot entity-num
              ;_ (println (str "getting " instruction ", so will look up: " bot))
              retrieved-bot (bots bot)
              new-bot (update retrieved-bot :values conj value)
              removed-state (if from-bot
                             (do
                               (assert (number? from-bot) (str "Not number: " (type from-bot)))
                    ;(println (str "to rem: " value " from " from-bot))
                    ;(println (str "from bot values before: " (:values (get bots from-bot))))
                               (-> bots
                                   (update-in [from-bot :values] (fn [old-values]
                                                        ;(println "b4:" old-values)
                                                                   (let [res (vec (remove #{value} old-values))]
                                                              ;_ (println "after:" res)

                                                                     res)))))

                             bots)
              new-state (-> removed-state
                            (assoc bot new-bot))]
              ;_ (when from-bot (println "from bot values after: " (:values (get new-state from-bot))))

          (if (> (-> new-bot :values count) 1)
            (let [bot-values (:values new-bot)
                  _ (assert (= 2 (count bot-values)) (str "More than 2: " new-bot))
                  lower (apply min bot-values)
                  higher (apply max bot-values)
                  _ (assert (not= lower higher))
                  together #{lower higher}
                  _ (when do-part-one (assert (not= together watch-compare) (str "bot " bot " is responsible for " watch-compare)))
                  low-receiver (:low retrieved-bot)
                  ;_ (println (str "low " lower " goes to " low-receiver))
                  high-receiver (:high retrieved-bot)
                  ;_ (println (str "high " higher " goes to " high-receiver))
                  changed-state-1 ((give-one new-state) {:from-bot bot :entity low-receiver :value lower})
                  changed-state-2 ((give-one changed-state-1) {:from-bot bot :entity high-receiver :value higher})]

              changed-state-2)
            new-state))
        (do
          (println (str "To output " value " to " entity))
          bots)))))


(defn x []
  (let [in (line-seq (io/reader (io/resource "ten.txt")))
        hiccup (map to-hiccup in)
        instructions (produce-instructions hiccup)
        bots (produce-bots-hash hiccup)
        new-bots (reduce
                   (fn [bots instruction]
                     ((give-one bots) instruction))
                   bots
                   instructions)]

    in))

(defn x-1 []
  (to-hiccup (second test-lines)))

;;
;; Bruce below
;;

(def test-lines ["value 5 goes to bot 2"
                 "bot 2 gives low to bot 1 and high to bot 0"
                 "value 3 goes to bot 1"
                 "bot 1 gives low to output 1 and high to bot 0"
                 "bot 0 gives low to output 2 and high to output 0"
                 "value 2 goes to bot 2"])

(defn pluck [pred list]
  [(filter pred list) (remove pred list)])

(defn pluck [pred list]
  ((juxt #(filter pred %) #(remove pred %)) list))

;;
;; No different to using line-seq (except returns a vector)
;;
(def data (string/split-lines (string/trim (slurp (io/resource "ten.txt")))))

;;
;; Grabs all from a line eg (:bot116 :bot157 :bot197)
;; Put the second 2 (rest) together into a keyword.
;; Second 2 are two brackets (as with re-find and re-match)
;;
(defn addr-names [l]
  (->> (re-seq #"(bot|output)\s(\d+)" l)
       (map (comp keyword #(apply str %) rest))))

;;
;; Each line is a value or a bot:
;; "value 5 goes to bot 2"
;; "bot 2 gives low to bot 1 and high to bot 0"
;;
(defn parse-line [l]
  (let [addrs (addr-names l)]
    (condp = (first l)
      \v  (vector :to (first addrs) (u/to-int (re-find #"\d+" l)))
      \b  (cons :from addrs))))

;; all commands originate at a unique bot
#_(let [froms (map second (filter #(= (first %) :from) input))]
    (= (count froms) (count (set froms))))

;;
;; addr key (eg :bot1 or :output1) has value which is a set of integers, where each
;; integer is a microchip value.
;;
(defn give-to-addr [registers addr v]
  ;(println v)
  (update registers addr (fnil conj #{}) v))

(defn move-value [registers from to v]
  {:pre [((get registers from) v)]} ;; must have val to give
  (-> registers
      (update from disj v)
      (give-to-addr to v)))

;;
;; first in any command is either :from or :to
;; :to will be eg [:to :bot77 7] [:to :bot113 11] [:to :bot162 59]
;; registers will end up with :bot1 having a #{} of microchip values
;; tos are the value lines, which are now becoming registers.
;;
(defn make-init [commands]
  (let [{:keys [from to]} (group-by first commands)]
    (println to)
    {:commands from
     :registers (reduce #(apply give-to-addr %1 (rest %2)) {} to)}))

(defn high-low-command [registers [_ from low-to high-to :as com]]
  (let [[lv hv] ((juxt first last) (sort (get registers from #{})))]
    (assert (and lv hv (not= lv hv)))
    (-> registers
        (move-value from low-to lv)
        (move-value from high-to hv))))

;;
;; If a bot only has one microchip then it can't do anything - must wait till has two.
;;
(defn active-registers [x] (->> x (filter #(>= (count (val %)) 2)) keys set))

;;
;; A command is something like "bot 2 gives low to bot 1 and high to bot 0". But this command is
;; only going to be activated when bot2 has 2 microchips.
;; The second thing in a command must be a bot or output number.
;; So when registers are activated we can pluck some commands
;;
(defn transition-state [{:keys [registers commands] :as state}]
  (let [active-regs                     (active-registers registers)
        [active-commands rest-commands] (pluck #(-> % second active-regs) commands)]
    (when-not (empty? active-commands)
      (-> state
          (update :registers #(reduce high-low-command % active-commands))
          (assoc :commands rest-commands)))))

;; part 1
(defn x-1 []
  (->> (iterate transition-state (make-init (map parse-line data)))
       (take-while #(not (nil? %)))
       (map :registers)
       (keep #(some (fn [[k v]] (when (empty? (difference #{61 17} v)) k)) %))
       first))
;;=> :bot161

;; part 2
(defn x-2 []
  (->> (iterate transition-state (make-init (map parse-line data)))
       (take-while #(not (nil? %)))
       last
       :registers
       (#(select-keys % [:output0 :output1 :output2]))
       vals
       (map first)
       (apply *)))
;; => 133163
