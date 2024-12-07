(ns day07-numeric
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(def input
  (for [line (str/split-lines (slurp *in*))
        :let [[tv & rest] (mapv parse-long (str/split line #"\:? "))]]
    [tv (reverse rest)]))

(dorun input)
(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn solve [^long tv [^long n & n*]]
  (if (seq n*)
    (or (when (zero? (rem tv n)) (solve (quot tv n) n*))
        (when (>= tv n)          (recur (- tv n) n*)))
    (= tv n)))

(->> input
     (filter (partial apply solve))
     (transduce (map first) +)
     (println 'First)
     (time)) ;; 4ms

(defn cutoff [^long number ^long suffix]
  (let [d (long (math/pow 10 (inc (int (math/log10 suffix)))))]
    (when (= suffix (rem number d))
      (quot number d))))

(defn solve [^long tv [^long n & n*]]
  (if (seq n*)
    (or (when (>= tv n) (solve (- tv n) n*))
        (when (zero? (rem tv n)) (solve (quot tv n) n*))
        (some-> (cutoff tv n) (-> long (recur n*))))
    (= tv n)))

(->> input
     (filter (partial apply solve))
     (transduce (map first) +)
     (println 'Second)
     (time)) ;; 7ms