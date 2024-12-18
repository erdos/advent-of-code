(ns day18
  (:require [loom.graph] [loom.alg]))

(declare splitsearch data)

(load-file "common.clj")

(def w 71)

(def input (partition 2 (read-string (str \[ data \]))))

(defn solve1 [n]
  (let [grid (reduce (fn [grid [x y]] (assoc-in grid [y x] \#))
                     (vec (repeat w (vec (repeat w \.))))
                     (take n input))]
    (-> (into {}
              (for [i (range w)
                    j (range w)
                    :let [loc [i j]]
                    :when (= \. (get-in grid loc))]
                [loc (for [n [[i (inc j)] [(inc i) j]]
                           :when (= \. (get-in grid n))]
                       n)]))
        (loom.graph/graph)
        (loom.alg/shortest-path [0 0] [(dec w) (dec w)])
        (count) (dec))))

(println :first-test (solve1 12)) ;; 22
(println :first-prod (solve1 1024))

(->> (splitsearch (comp neg? solve1) (vec (range 0 (count input))))
     (dec)
     (nth input)
     (println 'Second))