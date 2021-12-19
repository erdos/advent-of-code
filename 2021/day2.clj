#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn ->long [s] (Long/parseLong s))

(def input-lines (vec (s/split-lines (slurp "input2.txt"))))

(def lines
  (for [line input-lines
        :let [[_ direction distance] (re-matches #"(\w+) (\d+)" line)]]
    [(keyword direction) (->long distance)]))

(->> (reduce (fn [[hor depth] [cmd dist]]
               (case cmd
                 :forward [(+ hor dist) depth]
                 :down [hor (+ depth dist)]
                 :up [hor (- depth dist)]))
             [0 0] lines)
     (reduce *)
     (println "First answer: "))

(->> (reduce (fn [[hor depth aim] [cmd dist]]
               (case cmd
                 :forward [(+ hor dist) (+ depth (* aim dist)) aim]
                 :down [hor depth (+ aim dist)]
                 :up [hor depth (- aim dist)]))
             [0 0 0] lines)
     (take 2)
     (reduce *)
     (println "Second answer: "))
