(ns day10)

(def grid
  (->> (slurp *in*)
       (clojure.string/split-lines)
       (mapv (partial mapv (comp parse-long str)))))

(def trailheads
  (for [i (range (count grid))
        j (range (count (grid i)))
        :when (zero? (get-in grid [i j]))]
    [i j]))

(defn neighbors [location]
  (let [height (get-in grid location)]
    (for [delta [[-1 0] [1 0] [0 -1] [0 1]]
          :let [new-location (mapv + location delta)]
          :when (= (get-in grid new-location) (inc height))]
      new-location)))

(defn extend-path [path]
  (for [n (neighbors (first path))]
    (cons n path)))

(defn paths [trailhead]
  (nth (iterate (partial mapcat extend-path) [[trailhead]]) 9))

(defn trailhead-score [trailhead]
  (count (set (map first (paths trailhead)))))

(println 'First  (transduce (map trailhead-score) + trailheads))

(println 'Second (transduce (map (comp count paths)) + trailheads))
