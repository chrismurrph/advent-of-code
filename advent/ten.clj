(ns advent.ten
  (:require [instaparse.core :as insta]))

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

(defn x []
  (->> test-lines
       (map to-hiccup)))

(defn x-1 []
  (to-hiccup (second test-lines)))
