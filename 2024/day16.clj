(ns day16
  (:require [loom.graph :as g :refer [weighted-graph weighted-digraph]]
            [loom.alg :as alg :refer [dijkstra-path-dist dijkstra-span]]
            [clojure.string :as s]))

(declare grid)
(load-file "common.clj")

(def all-coords
  (for [i (range (count grid))
        j (range (count (grid i)))]
    [i j]))

(def start (first (filter #(= \S (get-in grid %)) all-coords)))
(def end (first (filter #(= \E (get-in grid %)) all-coords)))

(def neigh-dirs {:NORTH [:EAST :WEST]
                 :SOUTH [:EAST :WEST]
                 :EAST [:NORTH :SOUTH]
                 :WEST  [:NORTH :SOUTH]})

(def diff-dir {:NORTH [-1 0] :SOUTH [1 0] :EAST  [0 1] :WEST [0 -1]})

(def g (apply weighted-digraph
              (concat
               ;; rotate
               (for [[y x] all-coords
                     :when (not= (get-in grid [y x]) \#)
                     [d nd] neigh-dirs d2 nd]
                 [[y x d] [y x d2] 1000])
               ;; move
               (for [[y x] all-coords
                     :when (#{\S \.} (get-in grid [y x]))
                     [d diff]   diff-dir
                     :let [[yy xx] (mapv + [y x] diff)]
                     :when (#{\. \E} (get-in grid [yy xx]))]
                 [[y x d] (if (= [yy xx] end) [yy xx] [yy xx d]) 1]))))

(defn solve1 [g] (dijkstra-path-dist g [(start 0) (start 1) :EAST] end))

(time (let [[_ dist] (solve1 g)]
        (println 'First dist)
        (def best-distance dist)))

(def span (dijkstra-span g [(start 0) (start 1) :EAST]))

(def g-1 (apply weighted-digraph
                (for [[k v] (:adj g)
                      [target weight] v]
                  [target k weight])))

(def span-1 (dijkstra-span g-1 end))

(println (get span-1 end))
(println (span [2 13 :NORTH]) (span [1 12 :EAST]))
(->> (for [node (loom.graph/nodes g-1)
           [next-node weight1] (span node)
           [prev-node weight2] (span-1 node)
           :when (#{-2 -2000 -1001} (- best-distance (+ weight1 weight2)))]
       [(first node) (second node)])
     (set)
     (count)
     (inc)
     (println))