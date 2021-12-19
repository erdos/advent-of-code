#!/usr/bin/env bb

(def input (read-string (str "[" (slurp "input6.txt") "]")))

(defn live [x]
  [(x 1) (x 2) (x 3) (x 4) (x 5) (x 6)
   (+ (or (get x 7) 0) (or (get x 0) 0)) ; 6
   (x 8)
   (x 0)])

(defn solve [input steps]
  (reduce + (nth (iterate live (frequencies input)) steps)))

(println "First answer:" (solve input 80))
(println "Second answer:" (solve input 256))
