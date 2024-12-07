(ns day07
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(load-file "common.clj")

(def input
  (for [line lines
        :let [[tv & rest] (->> (str/split line #"\:? ") (mapv parse-long))]]
    [tv (reverse rest)]))

(defn solve [tv [n & n*]]
  (if (seq n*)
    (or (when (zero? (rem tv n)) (solve (quot tv n) n*))
        (when (>= tv n) (recur (- tv n) n*)))
    (= tv n)))

(->> input
     (filter (partial apply solve))
     (transduce (map first) +)
     (println 'First)
     (time))

(defn strlen [n] (inc (int (math/log10 n))))

(defn cutoff [x y]
  (when (str/ends-with? (str x) (str y))
    (parse-long (subs (str x) 0 (- (strlen x) (strlen y))))))

(defn solve [tv [n & n*]]
  (if (seq n*)
    (or (when (>= tv n) (solve (- tv n) n*))
        (when (zero? (rem tv n)) (solve (quot tv n) n*))
        (some-> (cutoff tv n) (recur n*)))
    (= tv n)))

(->> input
     (filter (partial apply solve))
     (transduce (map first) +)
     (println 'Second)
     (time))