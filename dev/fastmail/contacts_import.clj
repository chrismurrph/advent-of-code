(ns fastmail.contacts-import
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :refer [difference union intersection]]
            [utils :as u]))

;;
;; raw file saved directly to your HD from google, then need to use dos2unix on it
;; file -bi google_export.csv
;; text/plain; charset=utf-16le
;; After dos2unix:
;; text/plain; charset=utf-8
;; Note visible only in vi, text editors see gobeldy-gook
;;
(def real-file-name-2 "google_export.csv")
(def real-file-name-1 "fastmail_import.csv")
(def real-file real-file-name-2)
(def test-import "test_import")
(def my-input-file-name real-file-name-2)
(def my-output-file-name "output.txt")

;; Ignores used here won't apply to other situations
(def test-file? (= my-input-file-name test-import))

(def target-fastmail-headings
  #{"Title", "First Name", "Last Name", "Nick Name", "Company", "Department", "Job Title", "Business Street",
    "Business Street 2", "Business City", "Business State", "Business Postal Code", "Business Country",
    "Home Street", "Home Street 2", "Home City", "Home State", "Home Postal Code", "Home Country",
    "Other Street", "Other Street 2", "Other City", "Other State", "Other Postal Code", "Other Country",
    "Business Fax", "Business Phone", "Business Phone 2", "Home Phone", "Home Phone 2", "Mobile Phone",
    "Other Phone", "Pager", "Birthday", "E-mail Address", "E-mail 2 Address", "E-mail 3 Address",
    "Notes", "Web Page"})
(def target-test-headings
  #{"Name", "Given Name", #_"Additional Name", "Family Name"})
(def target-headings (if test-file? target-test-headings target-fastmail-headings))

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

(def test-file-translations {"Informal Name" "Name",
                             "First Name"    "Given Name",
                             ;"Slurr" "Additional Name",
                             "Surname"       "Family Name"})
(def real-file-translations {
                             ;;=> "Name"                             "Company"
                             "Given Name"                       "First Name"
                             "Additional Name"                  "Nick Name"
                             "Family Name"                      "Last Name"
                             "Name Prefix"                      "Title"
                             "Birthday"                         "Birthday"
                             "Organization 1 - Name"            "Company"
                             "Organization 1 - Department"      "Department"
                             "Address 1 - Street"               "Home Street"
                             "Address 1 - City"                 "Home City"
                             "Address 1 - Region"               "Home State"
                             "Address 1 - Postal Code"          "Home Postal Code"
                             "Address 1 - Country"              "Home Country"
                             "E-mail 1 - Value"                 "E-mail Address"
                             "Address 1 - PO Box"               "Home Street 2"
                             "Phone 2 - Value"                  "Home Phone 2"
                             "E-mail 2 - Value"                 "E-mail 2 Address"
                             "Address 1 - Extended Address"     "Home Street 2"
                             "Organization 1 - Type"            "Business City"
                             "Phone 3 - Value"                  "Other Phone"
                             "Phone 4 - Type"                   "Business Phone"
                             "Organization 1 - Yomi Name"       "Business Street 2"
                             "Phone 4 - Value"                  "Business Phone"
                             "Phone 1 - Value"                  "Home Phone"
                             "Organization 1 - Job Description" "Business State"
                             "Name Suffix"                      "Job Title"
                             "Organization 1 - Location"        "Business City"
                             ;;=> "Website 1 - Value"                "Web Page"
                             })

(def translations (if test-file? test-file-translations real-file-translations))

(def heading-translations (merge perfect-translations translations))

(let [trans-to-headings (-> heading-translations vals set)
      inventeds (difference trans-to-headings target-headings)]
  (assert (= #{} inventeds) (str "Can't make up a target heading: " (first inventeds))))

;;
;; Ones there's no mapping to, that we will loose the data of
;;
(def ignore-headings
  (if test-file?
    #{"Slurr"}
    #{"Group Membership"
      "Phone 1 - Type"
      "Address 1 - Type"
      "E-mail 1 - Type"
      "E-mail 2 - Type"
      "Phone 2 - Type"
      "Phone 3 - Type"
      "Billing Information"
      "Directory Server"
      "Mileage"
      "Occupation"
      "Location"
      "Hobby"
      "Sensitivity"
      "Priority"
      "Subject"
      ;;=> "Name"
      ;;=> "Website 1 - Value"
      "Initials"
      "Yomi Name"
      "Additional Name Yomi"
      "Family Name Yomi"
      "Given Name Yomi"
      "Website 1 - Type"
      "Nickname"
      "Gender"
      "Short Name"
      "Maiden Name"
      "Address 1 - Formatted"}))

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

(defn organise-row [make-translated-f value-populated-headings row-num]
  (let [accepted-count (count value-populated-headings)]
    (if (zero? accepted-count)
      {:row-num row-num :accepted-count accepted-count}
      (->> value-populated-headings
           (mapv make-translated-f)
           ;u/probe-on
           (group-by #((complement nil?) (:cell/to %)))
           (map (fn [[k v]] (if k [:translateds v] [:not-translateds v])))
           (into {:row-num row-num :accepted-count accepted-count})))))

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

(defn not-rubbish? [[from-heading cell-value]]
  (let [preds (map complement (assemble-cell-ignores from-heading))]
    ((apply every-pred preds) cell-value)))

(defn row-reader-hof [headings-from-to]
  (let [_ (println "orig size: " (count headings-from-to))
        make-translated-f (make-translated headings-from-to)
        all-from-headings (mapv first headings-from-to)
        _ (println (str "all-from-headings: " all-from-headings))
        _ (println (str "ignore-headings: " ignore-headings))
        from-headings (remove ignore-headings all-from-headings)
        accepted-positions (utils/positions (set from-headings) all-from-headings)
        _ (println "accepted positions: " accepted-positions)
        [from-to-sz ignore-sz from-sz] (map count [headings-from-to ignore-headings from-headings])
        ]
    (assert (or test-file? (= (- from-to-sz ignore-sz) from-sz))
            (str "S/have ended up with " (- from-to-sz ignore-sz) ", but remove of " ignore-sz " didn't work as left with: " from-sz))
    (fn read-row [row-num row-data]
      ;(println (str "row size: " (count row-data)))
      ;(println (str "<" (seq row-data) ">, row-num: " row-num))
      (assert (= (count row-data) (count headings-from-to)) (str "headings: " (count headings-from-to) ", row-data: " (count row-data)))
      (let [accepted-row (map (vec row-data) accepted-positions)
            ;_ (println accepted-row)
            rows-sz (count accepted-row)
            headings-sz (count from-headings)]
        (u/assrt (= rows-sz headings-sz) (str rows-sz " not= " headings-sz))
        (let [value-populated-headings (->> accepted-row
                                            (map vector from-headings)
                                            (filter not-rubbish?))
              ;_ (println (str "row " row-num " reduced from " (count accepted-row) " to " (count value-populated-headings)))
              organised-row (organise-row make-translated-f value-populated-headings row-num)
              _ (check-dups organised-row)]
          organised-row)))))

(defn score-at [headings-type-kw headings]
  (let [_ (println (str "head: " (seq headings)))
        good-paths (->> heading-translations headings-type-kw (map first) set)]
    (intersection (set headings) good-paths)))

(defn get-input-lines [file-name]
  (line-seq (io/reader (io/resource file-name)))
  )

(defn transpose [xss]
  (assert (= (count (first xss)) (count (second xss)) (count (#(nth % 2) xss))))
  (apply map vector xss))

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
    {:rest-line       (rest rest-line)
     :curr-position   (inc curr-position)
     :comma-positions new-comma-positions
     :in-quote?       new-in-quote}))

(defn append-ending [x positions]
  (conj positions (count x)))

;;
;; More complex because we don't count commas within quotes
;;
(defn split-by-comma [init-in-quote? x]
  (let [init-state {:rest-line       x
                    :curr-position   0
                    :comma-positions []
                    :in-quote?       init-in-quote?}]
    (->> (drop-while (complement finished?) (iterate step init-state))
         first
         :comma-positions
         (append-ending x)
         (into [-1])
         (partition 2 1)
         (map (fn [[y z]] (subs x (inc y) z)))
         )))

;;
;; In reality user will check ignores from the incoming headings. Every heading will either be
;; ignored or translated. So in UI this won't need to be called
;;
(defn check-all-ignoreds-exist [headings]
  (let [diff (clojure.set/difference ignore-headings (set headings))
        ;_ (println "DIFF" diff)
        okay? (or (= #{} diff) test-file?)]
    (assert okay? (str "These ignored headings don't exist, so don't need to be on ignored list: " diff))
    headings))

;;
;; Normal concatenation won't work as the line that opened the quote was not known about when did
;; the second line, so the commas were not seen. Hence we do the splitting by commas operation again.
;; Only issue left here is that the joining cell, which is last of x and first of y, has a double quote
;; at its beginning and end.
;; Didn't solve as this joining column is ignored for file dealing with.
;;
(defn -concat [x y]
  (if (= \" (-> x last first))
    (concat (butlast x) (split-by-comma false (apply str (cons (str (last x) ", ") y))))
    (concat x y)))

(defn maybe-join [num-headings [x y]]
  (let [x-sz (count x)
        y-sz (count y)]
    (cond
      (= num-headings x-sz y-sz) y
      (and (< x-sz num-headings) (< y-sz num-headings)) (-concat x y)
      (= num-headings x-sz) x
      (= num-headings y-sz) y)))

(defn desc [cells]
  (if (> (count cells) 5)
    (last cells)
    cells))

;;
;; Even with the raw exported file (post dos2unix) some lines are too short, spilling onto the next line.
;; So here we recognise them and join them together
;;
(defn join-short-lines [expected-sz lines-strs]
  ;(println (str "counts: " (vec (remove #(= (second %) expected-sz) (map-indexed (fn [idx line] [idx (count line) (desc line)]) lines-strs)))))
  (->> (cons (repeat expected-sz "") lines-strs)
       (partition 2 1)
       (map (partial maybe-join expected-sz))))

;;
;; Need to keep putting on ignores and translates, until an empty coll is returned
;; For every line a check is done that a non-blank cell value is either translated or ignored
;;
(defn non-translateds [headings-from-to lines]
  (let [row-reader-f (row-reader-hof headings-from-to)]
    (->> lines
         (map-indexed vector)
         (map (fn [idx row-data] (row-reader-f idx row-data)))
         (filter #(-> % second :not-translateds))
         (map #(-> second :not-translateds))
         (take 5)
         )))

(defn empties [headings-from-to lines]
  (let [row-reader-f (row-reader-hof headings-from-to)]
    (->> lines
         (map-indexed vector)
         (map (fn [[idx row-data]] (row-reader-f idx row-data)))
         (filter #(and (-> % :accepted-count pos?) (-> % :translateds empty?)))
         ;(map #(nth lines (:row-num %)))
         )))

;;
;; A completely blank input line, resulting in completely blank output line is not a problem,
;; because problems are those that can be fixed by changing translation and ignores.
;;
(defn all-blank? [row]
  (every? #(= % "") row)
  )

(defn translate [string-lines]
  (let [[headings-str & lines-strs] string-lines
        incoming-headings (mapv s/trim (s/split headings-str #","))
        translated-headings (map heading-translations incoming-headings)
        headings-from-to (mapv vector incoming-headings translated-headings)
        ;_ (println (str "headings: " headings))
        _ (check-all-ignoreds-exist incoming-headings)
        lines' (for [line-str lines-strs]
                 (split-by-comma false line-str))
        lines (join-short-lines (count headings-from-to) lines')]
    (let [problems (empties headings-from-to lines)]
      (if (seq problems)
        ["PROBLEMS" problems]
        (->> lines
             (cons translated-headings)
             transpose
             (remove #(nil? (first %)))
             transpose
             (remove all-blank?)
             )))))

;;
;; (->> old-data
;; (map change-line)
;; (interpose \newline)
;; (apply str)
;; (spit "output.txt"))
;;
(defn write-to-file [file-name records]
  (let [string-lines (map #(->> %
                                (interpose ",")
                                (apply str))
                          records)]
    (->> string-lines
         (interpose \newline)
         (apply str)
         (spit file-name))))

(defn x-1 []
  (->> my-input-file-name
       get-input-lines
       translate
       ;(write-to-file my-output-file-name)
       ))

