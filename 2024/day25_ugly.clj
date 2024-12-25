(ns day25-ugly)

(->> (.split   (slurp *in*) "\n\n")
     (map     #(vec (.split % "\n")))
     (map      (partial mapv vec))
     (group-by ffirst)
     (vals)
     (map      (partial map
                        (comp (partial mapv dec)
                              (partial map count)
                              (partial map (partial keep #{\#}))
                              (partial apply map vector)))) ; transpose
     (apply   #(for [a %1 b %2] [a b])) ; cross product
     (map      (partial apply map +))
     (filter   (partial every? #{0 1 2 3 4 5}))
     (count)
     (time)
     (println 'First))