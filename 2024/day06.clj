(ns day06)

(declare grid)
(load-file "common.clj")

(defn step-in [dir [i j]]
  (case dir
    :north [(dec i) j]
    :south [(inc i) j]
    :west  [i (dec j)]
    :east  [i (inc j)]))

(def right {:north :east, :east :south, :south :west, :west :north})

(def all-coords
  (for [i (range (count grid))
        j (range (count (first grid)))]
    [i j]))

(def inside? (set all-coords))

(def start-loc (some #(when (= (get-in grid %) \^) %) all-coords))

(defn step [grid [pos direction]]
  (let [nxt (step-in direction pos)]
    (if (= \# (get-in grid nxt))
      (recur grid [pos (right direction)])
      [nxt direction])))

(def visited-locs
  (->> [start-loc :north]
       (iterate (partial step grid))
       (map first)
       (take-while inside?)
       (set)
       (time))) ;; 5ms

(->> visited-locs
     (count)
     (println "First"))

(defn cyclic? [path]
  (= (count inside?) (count (take (count inside?) path))))

(->> (for [pos visited-locs
           :let [new-grid (assoc-in grid pos (first "#"))]]
       (->> [start-loc :north]
            (iterate (partial step new-grid))
            (take-while (comp inside? first))))
     (filter cyclic?)
     (count)
     (println "Second")
     (time)) ;; 31s
