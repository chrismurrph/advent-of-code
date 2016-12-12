(ns advent.eleven
  (:require [instaparse.core :as insta]))

(def example-in
  "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.
")

(def input ["The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator."
"The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip."
"The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip."
"The fourth floor contains nothing relevant."])

(def grammar-1
  (insta/parser
    "<S> = sentence
     ordinal = 'first' | 'second' | 'third' | 'fourth'
     nothing = <'nothing relevant'>
     word = #'[a-zA-Z]+'
     generator = word ' generator'
     microchip = word '-compatible microchip'
     item = generator | microchip
     items = (' a ' item ',') + ' and a ' item
     contents = (items | ' nothing relevant') '.'
     sentence = <'The '> ordinal <' floor contains'> contents"))

(defn to-hiccup [s]
  (->> (first (grammar-1 s))
       (insta/transform
         {:ordinal (fn [x]
                     (case x
                       "first" 1
                       "second" 2
                       "third" 3
                       "fourth" 4))})))

(defn x-1 []
  (to-hiccup (nth input 2)))


