(ns day05)

(load-file "common.clj")

(def seeds
    (-> lines first
        (.replace ":" " ")
        (read-numbers)
        next))

(def maps
  (->>
    (consuming (fn [xs]
      (when (seq xs)
        (let [head     (-> xs first (.split " ") first)
              [a tail] (split-with not-empty (next xs))
              a        (map read-numbers a)]
          [[(keyword head) a] (next tail)]))) (nnext lines))
      (into (array-map)))) ;; to keep order

;; interval: [dest-range-start source-range-start range-len]
(defn conv [unit intervals]
  (or (some (fn [[dest src len]]
              (when (<= src unit (+ src len 1))
                (+ dest (- unit src))))
            intervals)
    unit))

(defn converto [seed] (reduce conv seed (vals maps)))

(->> seeds
     (map converto)
     (apply min)
     (println "First"))

(def seed-ranges (partition 2 seeds))

;; returns [mapped-intersection & unmapped-intersections]
(defn intersect [[unit-start unit-len :as unit]
                 [int-dest int-src int-len :as rule]]
 (let [metszet (when (and (< unit-start (+ int-src int-len))
                          (< int-src (+ unit-start unit-len)))
                  [(+ int-dest (- (max unit-start int-src) int-src))                 
                   (- (min (+ unit-start unit-len) (+ int-src int-len))
                      (max unit-start int-src))])]
   (cond-> (vector metszet)
      ;; before: unit starts before start of interval
      (< unit-start int-src)
      (conj [unit-start (min unit-len (- int-src unit-start))])
      ;; unit ends after end of interval
      (< (+ int-src int-len) (+ unit-start unit-len))
      (conj [(max unit-start (+ int-src int-len))
             (- (+ unit-start unit-len) (max unit-start (+ int-src int-len)))]))))

;; return tuple of [mappeds unmappeds]
(defn conv-all-unmapped [rule units]
  (let [xs (for [u units] (intersect u rule))]
    [(keep first xs) (mapcat next xs)]))

;; given an interval, return list of mapped intrevals
(defn conv-range [units rule-key all-rules]
  (let [[mapped unmapped]
           (reduce (fn [[mappeds unmappeds] rule]
                     (let [[ms unmappeds] (conv-all-unmapped rule unmappeds)]
                       [(concat mappeds ms) unmappeds]))
                   [() units] all-rules)]
    (concat mapped unmapped)))

(->> (reduce-kv conv-range seed-ranges maps)
     (map first) (apply min)
     (println "Second: "))
