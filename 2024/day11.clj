(ns day11 (:require [clojure.math :refer [log10 pow]]))

(def input (read-string (str \[ (slurp *in*) \])))

(defn step [n]
  (cond (zero? n)              [1]
        (odd? (int (log10 n))) (let [d (int (pow 10 (quot (inc (int (log10 n))) 2)))]
                                 [(quot n d) (rem n d)])
        :else                  [(* 2024 n)]))

(-> input
    (->> (iterate (partial mapcat step)))
    (nth 25) (count)
    (->> (println 'First))
    (time))

(defn freq-step [rock-freq]
  (apply merge-with +
         (for [[rock-number old-count] rock-freq
               split-number (step rock-number)]
           {split-number old-count})))

(-> (frequencies input)
    (->> (iterate freq-step))
    (nth 75)
    (->> (vals) (reduce +))
    (->> (println 'Second))
    (time))