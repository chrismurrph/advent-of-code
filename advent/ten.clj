(ns advent.ten
  (:require [instaparse.core :as insta]
            [utils :as u]))

(def test-input-line "bot 75 gives low to bot 145 and high to bot 95")
(def test-lines ["value 5 goes to bot 2"
                 "bot 2 gives low to bot 1 and high to bot 0"
                 "value 3 goes to bot 1"
                 "bot 1 gives low to output 1 and high to bot 0"
                 "bot 0 gives low to output 2 and high to output 0"
                 "value 2 goes to bot 2"])

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
         {:int clojure.edn/read-string}))
  )

(defn produce-instructions [hiccup]
  (reduce
    (fn [acc [cmd & tail]]
      (case cmd
        :value-goes (let [[value bot-num] tail]
                      (conj acc {:bot bot-num :value value}))
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

;;
;; Returns a bots state after an instruction.
;;
(defn give-one [bots]
  ;(println bots)
  (fn [{:keys [from-bot bot value] :as instruction}]
    (let [_ (println "getting " instruction)
          retrieved-bot (bots bot)
          new-bot (update retrieved-bot :values conj value)
          _ (when from-bot
              (assert (number? from-bot) (str "Not number: " (type from-bot)))
              (println (str "to rem: " value " from " from-bot))
              ;(println (str "from bot values before: " (:values (get bots from-bot))))
              )
          new-state (-> bots
                        (assoc bot new-bot)
                        (update-in [from-bot :values] (fn [old-values]
                                                        ;(println "b4:" old-values)
                                                        (let [res (vec (remove #{value} old-values))
                                                              ;_ (println "after:" res)
                                                              ]
                                                          res))))
          ;_ (when from-bot (println "from bot values after: " (:values (get new-state from-bot))))
          ]
      (if (> (-> new-bot :values count) 1)
        (let [bot-values (:values new-bot)
              _ (assert (= 2 (count bot-values)) (str "More than 2: " new-bot))
              lower (apply min bot-values)
              higher (apply max bot-values)
              _ (assert (not= lower higher))
              low-receiver (:low retrieved-bot)
              _ (println (str "low " lower " goes to" low-receiver))
              high-receiver (:high retrieved-bot)
              _ (println (str "high " higher " goes to" high-receiver))
              changed-state-1 ((give-one new-state) {:from-bot bot :bot (:entity-num low-receiver) :value lower})
              changed-state-2 ((give-one changed-state-1) {:from-bot bot :bot (:entity-num high-receiver) :value higher})
              ]
          changed-state-2)
        (let []
          new-state)))))

(defn x []
  (let [hiccup (map to-hiccup test-lines)
        instructions (produce-instructions hiccup)
        bots (produce-bots-hash hiccup)
        new-bots (reduce
                   (fn [bots instruction]
                     ((give-one bots) instruction))
                   bots
                   instructions)
        ]
    new-bots))

(defn x-1 []
  (to-hiccup (second test-lines)))
