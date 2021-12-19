#!/usr/bin/env bb

(def input (read-string (str "[" (slurp "input7.txt") "]")))
(def total (apply max input))

(defn cost [a b]
  (Math/abs (- a b)))

(->> (for [t (range (inc total))]
       (reduce + (map (partial cost t) input)))
     (apply min)
     (println "First answer:"))

(defn cost [a b]
  (let [n (Math/abs (- a b))]
    (/ (* n (inc n)) 2)))

(->> (for [t (range (inc total))]
       (reduce + (map (partial cost t) input)))
     (apply min)
     (println "Second answer:"))
