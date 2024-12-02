(ns day02)

(load-file "common.clj")

(def grid
  (for [line lines]
    (read-string (str "[" line "]"))))

(defn incr? [xs] (every? #{1 2 3} (map - xs (next xs))))
(defn decr? [xs] (incr? (map - xs)))

(->> grid
     (filter (some-fn incr? decr?))
     (count)
     (println "First:"))

(defn drop-nth [n coll]
  (concat (take n coll) (drop (inc n) coll)))

(defn removed? [xs]
  (some (some-fn incr? decr?)
        (for [n (range (count xs))]
          (drop-nth n xs))))

(->> grid
     (filter #(or (incr? %) (decr? %) (removed? %)))
     (count)
     (println "Second:"))
