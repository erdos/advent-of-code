#!/usr/bin/env clojure

(ns day15.core
  (:require [loom.graph :refer [weighted-graph weighted-digraph]]
            [loom.alg :refer [dijkstra-path-dist]]
            [clojure.string :as s]))

(def lines (s/split-lines (slurp "input15.txt")))

(defn wrap [x]
  (assert (integer? x) (str "Not int " (pr-str x)))
  (if (= x 9) 1 (inc x)))

(assert (= 1 (wrap 9)))
(assert (= 2 (wrap 1)))

(def lines
  (mapv (partial mapv (comp #(Long/parseLong %) str)) lines))

(def size (count lines))

(defn risklev [x y]
  (cond (= x y (dec size))
        (get-in lines [y x])

        (= x size) Long/MAX_VALUE
        (= y size) Long/MAX_VALUE

        :else
        (+ (get-in lines [y x])
           (min (risklev (inc x) y)
                (risklev x (inc y))))))
(def risklev (memoize risklev))

(->> (- (risklev 0 0) (get-in lines [0 0]))
     (println "First answer:"))

;; second part - modify data we loaded

(defn rep-x [line]
  (vec (for [i (range 5)
             c line]
         (nth (iterate wrap c) i))))

(defn rep-y [lines]
  (vec (for [i (range 5)
             line lines]
         (nth (iterate (partial mapv wrap) line) i))))

(def lines (rep-y (mapv rep-x lines)))
(def size (count lines))

(def wg
  (reduce (fn [m [x y]]
            (assoc m [x y]
                   (-> {[(inc x) y] (get-in lines [(inc x) y])
                        [(dec x) y] (get-in lines [(dec x) y])
                        [x (inc y)] (get-in lines [x (inc y)])
                        [x (dec y)] (get-in lines [x (dec y)])}
                       ((fn [m] (into {} (for [[k v] m :when v] [k v])))))))
          {}
          (for [i (range size) j (range size)] [i j])))

(def wg (weighted-digraph wg))
(def answ (dijkstra-path-dist wg
                              [0 0]
                              [(dec size) (dec size)]))

(println "Second answer:" (second answ))
