(ns day01)

(load-file "common.clj")

(def all-coords
  (for [i (range (count grid))
        j (range (count (first grid)))]
    [i j]))

(defn same-neighbors [[y x :as pt]]
  (for [[dy dx] [[-1 0] [0 -1] [1 0] [0 1]]
        :let [x (+ x dx) y (+ y dy)]
        :when (= (get-in grid pt) (get-in grid [y x]))]
    [y x]))

(defn four-neighbors [pt]
  (for [d [[-1 0] [0 -1] [1 0] [0 1]]] (mapv + pt d)))

(defn flood-fill [get-neighbors position]
  (loop [q (list position)
         filled #{}]
    (if-let [[p & q] (seq q)]
      (if (filled q)
        (recur q filled)
        (recur (into q (remove filled (get-neighbors p)))
               (conj filled p)))
      filled)))

;; Builds a map where key is a point, value is an area identifier int
(defn identify-locations [get-neighbors points]
  (second (reduce (fn [[max-id loc->id] location]
                    (if (loc->id location)
                      [max-id loc->id]
                      [(inc max-id) (into loc->id (zipmap (flood-fill get-neighbors location) (repeat max-id)))]))
                  [0 {}]
                  points)))

(defn area->bounds [area]
  (set (for [pt area
             d [[-1 0] [1 0] [0 1] [0 -1]]
             :when (not (contains? area (mapv + pt d)))]
         (mapv + pt (map #(* 0.5 %) d)))))

(def areas
  (->> all-coords
       (group-by (identify-locations same-neighbors all-coords))
       (vals)
       (map set)))

(->> (transduce (map #(* (count %) (count (area->bounds %)))) + areas)
     (println 'First)
     (time))

(defn round1 [[y x]] [(int y) (int x)])
(defn round2 [[y x]] [(int (* (Math/signum y) (Math/round (Math/abs y))))
                      (int (* (Math/signum x) (Math/round (Math/abs x))))])

(defn split-bounds [pts]
  (assert (set? pts))
  (count (set (vals (identify-locations
                     (fn sbf [%]
                       (for [n (four-neighbors %)
                             :when (pts n)
                             :when (or (= (get-in grid (round1 %)) (get-in grid (round1 n)))
                                       (= (get-in grid (round2 %)) (get-in grid (round2 n))))]
                         n))
                     pts)))))

(defn fract? [x] (#{0.5 -0.5} (rem x 1)))

; (println (fract? 1.0) (fract? 1.5) (fract? -1.5))
 ;(println "!" (split-bounds #{[1.0 0.5] [2.0 0.5] [4.0 0.5] [5.0 0.5]}))

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
