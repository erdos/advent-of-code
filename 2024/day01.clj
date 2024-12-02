(ns day01)

(load-file "common.clj")

(def pairs
  (for [line lines]
    (read-string (str "[" line "]"))))

(->> (map - (sort (map first pairs)) (sort (map second pairs)))
     (map abs)
     (reduce +)
     (println "First star:"))

(def cnt (frequencies (map second pairs)))

(->> (map first pairs)
     (map #(* % (cnt % 0)))
     (reduce +)
     (println "Second star:"))
