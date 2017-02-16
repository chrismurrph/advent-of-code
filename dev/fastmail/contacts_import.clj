(ns fastmail.contacts-import
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [difference union intersection]]
            [utils :as u]))

(def real-file-name "fastmail_import.csv")
(def test-import "test_import")
(def my-file-name real-file-name)

(def target-headings #{"Title","First Name","Last Name","Nick Name","Company","Department","Job Title","Business Street",
                     "Business Street 2","Business City","Business State","Business Postal Code","Business Country",
                     "Home Street","Home Street 2","Home City","Home State","Home Postal Code","Home Country",
                     "Other Street","Other Street 2","Other City","Other State","Other Postal Code","Other Country",
                     "Business Fax","Business Phone","Business Phone 2","Home Phone","Home Phone 2","Mobile Phone",
                     "Other Phone","Pager","Birthday","E-mail Address","E-mail 2 Address","E-mail 3 Address",
                     "Notes","Web Page"})

(defn blank? [value] (or (nil? value) (= "" value)))
;(defn only-non-letters? [value] (re-find #" *" value))
(defn every-char-special? [value] (every? #{\* \space} value))

;;
;; There are ignores for each from-heading.
;; So "E-mail 1 - Type" might have [blank? only-non-letters?]
;; Most would have these two as defaults.
;; The theory is that each from-heading is given defaults, that can then
;; be altered by the user.
;; Obviously we only know the from-heading when given the file to import.
;;
(defn assemble-cell-ignores [from-heading]
  [blank? every-char-special?])

;;
;; Here heading we actually get can be mapped to one of the doco headings
;;
(def perfect-translations (into {} (map (juxt identity identity) target-headings)))

(def heading-translations (merge perfect-translations
                                 {
                                "Given Name" "First Name"
                                "Additional Name" "Nick Name"
                                "Family Name" "Last Name"
                                "Name Prefix" "Title"
                                "Birthday" "Birthday"
                                "Organization 1 - Name" "Company"
                                "Organization 1 - Department" "Department"
                                ;"Organization 1 - Title" "Job Title"
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
                                ;"Organization 1 - Symbol" "Business City"
                                "Organization 1 - Job Description" "Business State"
                                ;"Yomi Name" "Last Name"
                                ;"Website 1 - Type" "Business Postal Code"
                                ;"Website 1 - Value" "Company"
                                "Name Suffix" "Job Title"
                                ;"Initials" "Nick Name"
                                "Organization 1 - Location" "Business City"
                                ;"Additional Name Yomi" "Home Postal Code"
                                ;"Family Name Yomi" "Last Name"
                                }))

(let [trans-to-headings (-> heading-translations vals set)
      inventeds (difference trans-to-headings target-headings)]
  (assert (= #{} inventeds) (str "Can't make up a Fastmail heading: " (first inventeds))))

;;
;; Ones there's no mapping to, that we will loose the data of
;;
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
                       "Name"
                       "Initials"
                       "Yomi Name"
                       "Additional Name Yomi"
                       "Family Name Yomi"
                       "Given Name Yomi"
                       "Organization 1 - Symbol"
                       "Website 1 - Type"
                       "Website 1 - Value"
                       "Organization 1 - Title"
                       "Nickname"
                       "Gender"
                       "Short Name"
                       "Maiden Name"
                       })

(let [in-common (intersection ignore-headings (-> heading-translations keys set))]
  (assert (= #{} in-common) (str "Can't ignore and have a translate for: " (first in-common))))

(defn make-translated [headings-from-to]
  (fn [[from-heading value]]
    (let [to-heading (get (into {} headings-from-to) from-heading)
          ;_ (println (str headings-from-to ", " from-heading))
          ]
      (if to-heading
        {:cell/from from-heading :cell/to to-heading :cell/value value}
        {:cell/from from-heading :cell/value value}))))

(defn organise-row [make-translated-f populated-headings]
  (->> populated-headings
       (mapv make-translated-f)
       ;u/probe-on
       (group-by #((complement nil?) (:cell/to %)))
       (map (fn [[k v]] (if k [:translateds v] [:not-translateds v])))
       (into {})))

(defn overwritten-column? [freqs]
  (when (seq freqs)
    (let [max-freq (apply max (vals freqs))]
      (> max-freq 1))))

(defn check-dups [row]
  (let [tos (map :cell/to (:translateds row))
        bad-row? (-> tos
                     frequencies
                     overwritten-column?)]
    (when bad-row?
      (println (str "Duplicated column in row: " (:translateds row))))))

(defn row-reader-hof [headings-from-to]
  (let [_ (println "orig size: " (count headings-from-to))
        make-translated-f (make-translated headings-from-to)
        all-from-headings (map first headings-from-to)
        from-headings (remove ignore-headings all-from-headings)
        accepted-positions (utils/positions (set from-headings) all-from-headings)
        _ (println "positions: " accepted-positions)
        ]
    (assert (= (- (count headings-from-to) (count ignore-headings)) (count from-headings)))
    (fn read-row [row-data]
      (let [_ (println (count row-data))
            _ (println row-data)
            ;accepted-row (map row-data accepted-positions)
            ;_ (println accepted-row)
            rows-sz (count row-data)
            headings-sz (count from-headings)]
        (assert (= rows-sz headings-sz) (str rows-sz " not= " headings-sz))
        (let [populated-headings (->> row-data
                                      (map vector from-headings)
                                      (filter (fn [[from-heading value]]
                                                (let [preds (map complement (assemble-cell-ignores from-heading))]
                                                  ((apply every-pred preds) value)))))
              organised-row (organise-row make-translated-f populated-headings)
              _ (check-dups organised-row)]
          organised-row)))))

(defn score-at [headings-type-kw headings]
  (let [_ (println (str "head: " (seq headings)))
        good-paths (->> heading-translations headings-type-kw (map first) set)]
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

(defn split-by-comma-simple [x]
  (s/split x #","))

(defn finished? [{:keys [rest-line]}]
  (empty? rest-line))

(defn step [{:keys [rest-line curr-position comma-positions in-quote?]}]
  (let [curr-val (first rest-line)
        new-in-quote (if (= curr-val \")
                       (not in-quote?)
                       in-quote?)
        new-comma-positions (if (and (not in-quote?) (= curr-val \,))
                              (conj comma-positions curr-position)
                              comma-positions)]
    {:rest-line (rest rest-line)
     :curr-position (inc curr-position)
     :comma-positions new-comma-positions
     :in-quote? new-in-quote}))

(defn append-ending [x positions]
  (conj positions (count x)))

(defn split-by-comma [x]
  (let [init-state {:rest-line x
                    :curr-position 0
                    :comma-positions []
                    :in-quote? false}]
    (->> (drop-while (complement finished?) (iterate step init-state))
         first
         :comma-positions
         (append-ending x)
         (into [-1])
         (partition 2 1)
         (map (fn [[y z]] (subs x (inc y) z)))
         )))

;;
;; Need to keep putting on ignores and translates, until an empty coll is returned
;;
(defn non-translateds []
  (let [[headings-str & lines-strs] (get-input-lines my-file-name)
        headings (s/split headings-str #",")
        translated-headings (map heading-translations headings)
        headings-from-to (mapv vector headings translated-headings)
        lines (for [line-str lines-strs]
                (split-by-comma line-str))
        row-reader-f (row-reader-hof headings-from-to)
        ]
    (->> lines
         (map #(row-reader-f %))
         (filter :not-translateds)
         (map :not-translateds)
         (take 5)
         )))

