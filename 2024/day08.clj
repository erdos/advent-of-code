(ns day08)

(->> (slurp *in*)
     (clojure.string/split-lines)
     (mapv vec)
     (def grid))

(def antenna-locs
  (for [i (range (count grid))
        j (range (count (grid i)))
        :when (not= \. (get-in grid [i j]))]
    [i j]))

(def locations (vals (group-by (partial get-in grid) antenna-locs)))

(defn in-bounds? [[i j]]
  (and (< -1 i (count grid)) (< -1 j (count (grid 0)))))

(defn pairs [xs]
  (for [[t & ts] (iterate next xs) :while t, u ts]
    [t u]))

(defn mirrors [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1) dy (- y2 y1)]
    (list [(- x1 dx) (- y1 dy)] [(+ x2 dx) (+ y2 dy)])))

(->> (transduce (comp (mapcat pairs)
                      (mapcat (partial apply mirrors))
                      (filter in-bounds?)
                      (distinct)
                      (map (constantly 1)))
                + locations)
     (println 'First)
     (time))

(defn harmonics [[x1 y1] [x2 y2] d]
  (for [i (range)] [(- x1 (* i (- x2 x1) d))
                    (- y1 (* i (- y2 y1) d))]))

(->> (for [locs locations
           [loc1 loc2] (pairs locs)
           d [-1 +1]
           m (take-while in-bounds? (harmonics loc1 loc2 d))]
       m)
     (set)
     (count)
     (println 'Second)
     (time))
