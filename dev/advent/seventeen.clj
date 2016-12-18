(ns advent.seventeen
  (:import (java.security MessageDigest)))

(defn breadth-first-search [max-steps
                            starting-lab
                            generate-possibilities
                            destination-state?
                            short-circuit-detector-fn?]
  (loop [already-tested #{starting-lab}
         last-round #{starting-lab}
         times 0
         most-distant-candidates nil]
    (assert (set? already-tested))
    (if (< times max-steps)
      (let [_ (println "steps done:" times "visited:" (count already-tested))
            newly-generated (mapcat generate-possibilities last-round)]
        (if (seq newly-generated)
          (if (not short-circuit-detector-fn?)
            (let [got-there? (first (filter destination-state? newly-generated))]
              (if got-there?
                (do
                  (println (str "Got there with: <" got-there? ">"))
                  {:steps times :res got-there?})
                (let [now-tested (into already-tested newly-generated)]
                  (recur now-tested (into #{} (remove already-tested newly-generated)) (inc times) most-distant-candidates))))
            (let [
                  ;;
                  ;; We want to overwrite/save the can-get-theres until we reach situation where there are no more
                  ;; possibilities. Then these saved paths are the ways to get there via the longest route.
                  ;;
                  ;; A problem remaining is that there are always more possibilities if can re-trace your route - just
                  ;; go around in a circle. So to avoid this problem we need to detect the circle.
                  ;;
                  non-short-new-gens (remove short-circuit-detector-fn? newly-generated)
                  can-get-theres (filter destination-state? newly-generated)]
              (let [now-tested (into already-tested non-short-new-gens)]
                (recur now-tested (into #{} (remove already-tested non-short-new-gens)) (inc times)
                       (or (seq can-get-theres) most-distant-candidates)))))
          [:dead-end "Nowhere to go, not even back where came from" already-tested]))
      [:need-more-steps "Need give more steps then run again" already-tested])))

(defn- md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(def -example-passcode "hijkl")
(def -real-passcode "yjjvjgan")
(def -part-2-example-1 "ihgpwlah")
(def passcode -real-passcode)

(defn open? [ch]
  (boolean (#{\b \c \d \e \f} ch)))

(defn up [[x y]]
  [x (dec y)])

(defn down [[x y]]
  [x (inc y)])

(defn left [[x y]]
  [(dec x) y])

(defn right [[x y]]
  [(inc x) y])

(def dir-fs {\U up
             \D down
             \L left
             \R right})

(defn go-there [position direction]
  (let [f (dir-fs direction)]
    (f position)))

;;
;;
;;
(defn create-positions-path [position directions]
  (reduce
    (fn [acc ele]
      (let [new-pos (go-there (last acc) ele)]
        (conj acc new-pos)))
    [position]
    directions))

(defn in-bounds? [width height [x y]]
  (if (or (neg? x) (neg? y))
    false
    (and (< x width) (< y height))))

(defn blocked-hof [width height]
  (fn [position]
    (fn [direction]
      (let [maybe-new-place (go-there position direction)]
        (not (in-bounds? width height maybe-new-place))))))

;;
;; The path in might be [\U \L], and out might be [[\U \L \R] [\U \L \D]]
;; For the position [0 0] is in top left corner and [(dec width) (dec height)] is in bottom right.
;;
(defn gen-paths [passcode width height]
  (let [no-go-f (blocked-hof width height)]
    (fn [[path position]]
      (let [blocked-move-f? (no-go-f position)
            to-hash (apply str passcode path)
            hash (take 4 (md5 to-hash))
            possible-opens (map open? hash)
            directions (filter identity (map (fn [truth? dir]
                                               (when truth? dir)) possible-opens [\U \D \L \R]))
            good-directions (remove blocked-move-f? directions)]
        ;(println directions good-directions)
        (for [direction good-directions
              :let [new-path (conj path direction)
                    res [new-path (go-there position direction)]]]
          res)))))

(def width 4)
(def height 4)

(defn x-3 []
  ((gen-paths passcode width height) [[\D] [0 1]]))

#_(defn x-2 []
    (let [formula (coord->wall? fav-num)]
      ((succ-loc? formula) [1 1])))

(def started-path-pos [[] [0 0]])

(defn stop-loc? [[path position]]
  (= position [3 3]))

(defn detect-short-circuit? [positions-path]
  (let [
        ;_ (assert false positions-path)
        freqs (frequencies (first positions-path))
        short-exists? (some #(> % 1) (vals freqs))]
    short-exists?))

(defn create-path [start-from]
  (fn [directions]
    (create-positions-path start-from directions)))

(defn x []
  (let [result (breadth-first-search 300
                                     started-path-pos
                                     (gen-paths passcode width height)
                                     stop-loc?
                                     nil #_detect-short-circuit?)
        res (:res result)]
    (if res
      (->> res first (apply str))
      (case (first result)
        :dead-end
        (let [
              ;_ (println (last result))
              longest (reduce (fn [acc ele]
                                (let [size (count ele)]
                                  (if (> size (count acc))
                                    ele
                                    acc)))
                              []
                              (map first (last result)))]
          longest)

        :need-more-steps (second result)

        :reached-end (second result)))))

(defn x-2 []
  (detect-short-circuit? [((create-path [0 0]) [\D \R \R \U \L \D \U \D \D \D \U \R \R \D \U]) :sumfin]))
