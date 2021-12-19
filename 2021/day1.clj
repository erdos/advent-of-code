#!/usr/bin/env bb

(def input (map read-string (line-seq (clojure.java.io/reader "input1.txt"))))

(defn inccount [input]
  (count (filter true? (map < input (next input)))))

(println "First answer: " (inccount input))

(def windows (map + input (next input) (nnext input)))

(println "Second answer: " (inccount windows))
