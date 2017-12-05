(ns user
  (:require [clojure.tools.namespace.repl :refer (refresh refresh-all)]
            [clojure.stacktrace :refer [print-stack-trace]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;;
;;
(defn reset []
  ;(use 'clojure.stacktrace)
  (refresh))

;;
;;
(defn stop-reset []
  (reset))
