(ns day25)

(def grids
  (for [input (.split (slurp *in*) "\n\n")]
    (mapv vec (.split input "\n"))))

(defn pin-heights [grid]
  (->> grid
       (apply map vector) ;; transpose
       (map (partial keep #{\#}))
       (map count)
       (mapv dec)))

(def locks+keys (group-by ffirst grids))

(->> (for [a (map pin-heights (locks+keys \#))
           b (map pin-heights (locks+keys \.))
           :when (not-any? (partial < 5) (map + a b))]
       1)
     (reduce +)
     (println 'First))