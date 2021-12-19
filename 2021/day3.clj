#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn ->long [s] (Long/parseLong s))

(def input-lines (vec (s/split-lines (slurp "input3.txt"))))

(def lines
  (mapv (comp (partial mapv #(%1 %2) [keyword ->long])
              next
              (partial re-matches #"(\w+) (\d+)"))
        input-lines))

(def len (count (first input-lines)))

(defn null-safe [m] (fn [k] (m k 0)))

(defn keep-common [input-lines]
  (let [len (count (first input-lines))]
    (apply str
           (map (fn [xs] (apply max-key (null-safe xs) "01"))
                (for [i (range len)]
                  (frequencies (for [line input-lines]
                                 (nth line i))))))))

(assert (= "11" (keep-common ["10" "01"])))
(assert (= "10" (keep-common ["11" "10" "00"])));; common

(def commonest (keep-common input-lines))

; (println "Csadf" (Long/parseLong commonest 2))

(defn keep-least [input-lines]
  (let [len (count (first input-lines))]
    (apply str
           (map (fn [xs] (apply min-key (null-safe xs) "10"))
                (for [i (range len)]
                  (frequencies (for [line input-lines] (nth line i))))))))

(assert (= "00" (keep-least ["01" "10"])))
(assert (= "01" (keep-least ["11" "10" "00"])))

(def co2-rating
 (reduce (fn [lines pos]
           (if (= 1 (count lines))
             (reduced lines)
             (let [digit (nth (keep-least lines) pos)]
               (for [line lines
                     :when (= (nth line pos) digit)]
                 line))))
         input-lines
         (range len)))

(def o2-rating
 (reduce (fn [lines pos]
           (if (= 1 (count lines))
             (reduced lines)
             (let [digit (nth (keep-common lines) pos)]
               (for [line lines
                     :when (= (nth line pos) digit)]
                 line))))
         input-lines
         (range len)))

(println "Second answer:" (* (Long/parseLong (first co2-rating) 2)
                             (Long/parseLong (first o2-rating) 2)))
