#!/usr/bin/env bb

(def input (read-string (str "[" (slurp "input7.txt") "]")))
(def total (apply max input))

(defn solve [cost input total]
  (transduce (map (fn [t] (transduce (map (partial cost t)) + input)))
             min
             Long/MAX_VALUE
             (range (inc total)))

 ;; without transducers:
  #_(->> (for [t (range (inc total))]
         (reduce + (map (partial cost t) input)))
       (apply min)))

(defn cost [a b]
  (let [d (- a b)] (if (pos? d) d (- d))))

(println "First answer: " (solve cost input total))

(defn cost2 [a b]
  (let [n (cost a b)]
    (/ (* n (inc n)) 2)))

(println "Second answer" (solve cost2 input total))
