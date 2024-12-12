(ns day12)

(->> (slurp *in*)
     (clojure.string/split-lines)
     (mapv (partial mapv vector))
     (def grid))

(def all-coords
  (for [y (range (count grid))
        x (range (count (first grid)))]
    [y x]))

(defn four-neighbors [point]
  (for [d [[-1 0] [0 -1] [1 0] [0 1]]] (mapv + d point)))

;; seq of neighboring points with the same value
(defn similar-neighbors [point]
  (filter (comp #{(get-in grid point)} (partial get-in grid))
          (four-neighbors point)))

(defn flood-fill [get-neighbors start]
  (loop [pts (list start)
         filled #{}]
    (if-let [[p & pts] (seq pts)]
      (if (filled pts)
        (recur pts filled)
        (recur (into pts (remove filled (get-neighbors p))) (conj filled p)))
      filled)))

;; Builds a map where key is a location, value is an area identifier int
(defn identify-locations [get-neighbors points]
  (second (reduce (fn [[max-id loc->id] location]
                    (if (loc->id location)
                      [max-id loc->id]
                      [(inc max-id) (into loc->id (zipmap (flood-fill get-neighbors location) (repeat max-id)))]))
                  [0 {}]
                  points)))

;; Given a set of points of an area, calculate set of boundary points (with fractional coordinates)
(defn area->bounds [area]
  (set (for [pt area
             d [[-1 0] [1 0] [0 1] [0 -1]]
             :when (not (contains? area (mapv + pt d)))]
         (mapv + pt (map #(* 0.5 %) d)))))

;; Coll of sets of points belonging to the same area
(def areas
  (->> all-coords
       (group-by (identify-locations similar-neighbors all-coords))
       (vals)
       (map set)))

(->> (transduce (map #(* (count %) (count (area->bounds %)))) + areas)
     (println 'First)
     (time))

(defn left-side  [[y x]] (get-in grid [(int y) (int x)]))
(defn right-side [[y x]] (get-in grid [(int (* (Math/signum y) (Math/round (Math/abs y))))
                                   (int (* (Math/signum x) (Math/round (Math/abs x))))]))

;; given a set of colinear points, count how many distinct edges they form
(defn split-bounds [pts]
  (assert (set? pts))
  (->> pts
       (identify-locations
        (fn sbf [p] (->> (four-neighbors p)
                         (filter pts)
                         (filter (some-fn (comp #{(left-side p)} left-side)
                                          (comp #{(right-side p)} right-side))))))
       (vals) (set) (count)))

(defn fract? [x] (#{0.5 -0.5} (rem x 1)))

(defn area->side-count [area]
  (assert (set? area))
  (->> (area->bounds area)
       (group-by (fn [[y x]] (cond (fract? x) [:x x] (fract? y) [:y y])))
       (vals)
       (map set)
       (map split-bounds)
       (reduce +)))

(->> (transduce (map #(* (count %) (area->side-count %)))
                + areas)
     (println 'Second)
     (time))
