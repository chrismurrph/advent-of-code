(ns fastmail.contacts-import
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [difference union intersection]]))

(def real-file-name "fastmail_import.csv")
(def test-import "test_import")
(def my-file-name real-file-name)

(def doco-headings #{"Title","First Name","Last Name","Nick Name","Company","Department","Job Title","Business Street",
                     "Business Street 2","Business City","Business State","Business Postal Code","Business Country",
                     "Home Street","Home Street 2","Home City","Home State","Home Postal Code","Home Country",
                     "Other Street","Other Street 2","Other City","Other State","Other Postal Code","Other Country",
                     "Business Fax","Business Phone","Business Phone 2","Home Phone","Home Phone 2","Mobile Phone",
                     "Other Phone","Pager","Birthday","E-mail Address","E-mail 2 Address","E-mail 3 Address",
                     "Notes","Web Page"})

(defn blank? [value] (or (nil? value) (= "" value)))
;(defn only-non-letters? [value] (re-find #" *" value))
(defn every-char-special? [value] (every? #{\* (first " ")} value))

;;
;; There are ignores for each from-heading.
;; So "E-mail 1 - Type" might have [blank? only-non-letters?]
;; Most would have these two as defaults.
;; The theory is that each from-heading is given defaults, that can then
;; be altered by the user.
;; Obviously we only know the from-heading when given the file to import.
;;
(defn assemble-ignores [from-heading]
  [blank? every-char-special?])

;;
;; Here heading we actually get can be mapped to to one of the doco headings
;;
(def perfect-headings (into {} (map (juxt identity identity) doco-headings)))

(def candidate-headings (merge perfect-headings
                               {
                                "Name" "First Name"
                                "Given Name" "First Name"
                                "Given Name Yomi" "First Name"
                                "Additional Name" "Nick Name"
                                "Family Name" "Last Name"
                                "Name Prefix" "Title"
                                "Birthday" "Birthday"
                                "Organization 1 - Name" "Company"
                                "Organization 1 - Department" "Department"
                                "Organization 1 - Title" "Job Title"
                                "Address 1 - Street" "Home Street"
                                "Address 1 - City" "Home City"
                                "Address 1 - Region" "Home State"
                                "Address 1 - Postal Code" "Home Postal Code"
                                "Address 1 - Country" "Home Country"
                                "E-mail 1 - Value" "E-mail Address"
                                "Address 1 - Formatted" "Home Street"
                                "Address 1 - PO Box" "Home Street 2"
                                "E-mail 2 - Type" "E-mail Address"
                                "Phone 2 - Value" "Home Phone 2"
                                "E-mail 2 - Value" "E-mail 2 Address"
                                "Address 1 - Extended Address" "Home Street 2"
                                "Organization 1 - Type" "Business City"
                                "Phone 3 - Value" "Other Phone"
                                "Phone 4 - Type" "Business Phone"
                                "Organization 1 - Yomi Name" "Business Street 2"
                                "Phone 4 - Value" "Business Phone"
                                "Phone 1 - Value" "Home Phone"
                                "Organization 1 - Symbol" "Business City"
                                "Organization 1 - Job Description" "Business State"
                                "Yomi Name" "Last Name"
                                "Website 1 - Type" "Business Postal Code"
                                "Website 1 - Value" "Company"
                                "Name Suffix" "Job Title"
                                "Initials" "Nick Name"
                                "Organization 1 - Location" "Business City"
                                "Additional Name Yomi" "Home Postal Code"
                                }))

(let [trans-to-headings (-> candidate-headings vals set)
      inventeds (difference trans-to-headings doco-headings)]
  (assert (= #{} inventeds) (str "Can't make up a Fastmail heading: " (first inventeds))))

(def ignore-headings #{"Group Membership"
                       "Phone 1 - Type"
                       "Address 1 - Type"
                       "E-mail 1 - Type"
                       "Phone 2 - Type"
                       "Phone 3 - Type"
                       "E-mail 3 - Type"
                       "E-mail 3 - Value"
                       "Billing Information"
                       "Directory Server"
                       "Mileage"
                       "Occupation"
                       "Location"
                       "Hobby"
                       "Sensitivity"
                       "Priority"
                       "Subject"
                       })

(let [in-common (intersection ignore-headings (-> candidate-headings keys set))]
  (assert (= #{} in-common) (str "Can't ignore and have a translate for: " (first in-common))))

(defn make-translated [headings-from-to]
  (fn [[from-heading value]]
    (let [to-heading (get (into {} headings-from-to) from-heading)
          ;_ (println (str headings-from-to ", " from-heading))
          ]
      (if to-heading
        {:cell/from from-heading :cell/to to-heading :cell/value value}
        {:cell/from from-heading :cell/value value}))))

(defn row-reader-hof [headings-from-to]
  (let [make-translated-f (make-translated headings-from-to)
        from-headings (->> headings-from-to
                           (map first)
                           (remove ignore-headings))]
    (fn read-row [row-data]
      (let [populated-headings (->> row-data
                                    (map vector from-headings)
                                    (filter (fn [[from-heading value]]
                                              (let [preds (map complement (assemble-ignores from-heading))]
                                                ((apply every-pred preds) value)))))]
        (->> populated-headings
             (mapv make-translated-f)
             (group-by #((complement nil?) (:cell/to %)))
             (map (fn [[k v]] (if k [:translateds v] [:not-translateds v])))
             (into {}))))))

(defn score-at [headings-type-kw headings]
  (let [_ (println (str "head: " (seq headings)))
        good-paths (->> candidate-headings headings-type-kw (map first) set)]
    (intersection (set headings) good-paths)))

(defn get-input-lines [file-name]
  (line-seq (io/reader (io/resource file-name))))

(defn transpose-1 [xss]
  (assert (= (count (first xss)) (count (second xss)) (count (#(nth % 2) xss))))
  (apply mapv vector xss))

(defn heading-has-data? [xs]
  (some #(and (not= nil %) (not= "" %)) (rest xs)))

(defn pad [xs n]
  (let [diff (- n (count xs))]
    (vec (concat xs (repeat diff nil)))))

(defn split-by-comma [x]
  (s/split x #","))

(defn x-1 []
  (let [[headings-str & lines-strs] (get-input-lines my-file-name)
        headings (s/split headings-str #",")
        translated-headings (map candidate-headings headings)
        headings-from-to (mapv vector headings translated-headings)
        lines (for [line-str lines-strs]
                (s/split line-str #","))
        row-reader-f (row-reader-hof headings-from-to)
        ]
    (->> lines
         (map #(row-reader-f %))
         (filter :not-translateds)
         (map :not-translateds)
         (take 5)
         )))

(defn x-2 []
  (let [lines-strs (get-input-lines my-file-name)
        sz (-> lines-strs first split-by-comma count)
        lines (for [line-str lines-strs]
                (pad (split-by-comma line-str) sz))
        ;actual-headings (first lines)
        ;_ (println (filter #(.startsWith (.toLowerCase %) "e-mail") actual-headings))
        ;_ (println headings)
        heading-leading (transpose-1 lines)
        actual-headings (map first heading-leading)
        with-substance (filter heading-has-data? actual-headings)
        ;_ (println (map-indexed vector (map first with-substance)))
        _ (println (str "Down from " (count heading-leading) " to "
                        (count with-substance) " to "
                        (count (score-at :my-google with-substance)) " or "
                        (count (score-at :perfect with-substance))))
        m (into {} (map (juxt first #(-> % rest vec)) with-substance))
        ]
    ;(mapv m ["Given Name" "Given Name Yomi"])
    (score-at :my-google with-substance)
    ))

(defn x-3 []
  (pad [1 2 3] 10))
