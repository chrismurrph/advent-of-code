(ns scratch)

(def m '({:id 1 :text "Buy bread"} {:id 2 :text "Pay taxes"}))

;; To:
;; {
;; 1 {:id 1 :text "Buy bread"}
;; 2 {:id 2 :text "Pay taxes"}}
;; }

(defn x-1 []
  (->> (map (fn [{:keys [id] :as val}]
              [id val])
            m)
       (into {})))

(defn x-2 []
  (reduce #(assoc %1 (:id %2) %2) {} m))
