(ns day20)

(->> (slurp *in*)
     (clojure.string/split-lines)
     (mapv vec)
     (def grid))

(def start (first (for [i (range (count grid))
                        j (range (count (grid i)))
                        :when (= \S (get-in grid [i j]))]
                    [i j])))

;; Seq of [y x] coordinates from start to end, inclusive.
(def full-path
  (loop [seen #{}
         [pos :as path] (list start)]
    (if (= \E (get-in grid pos))
      path
      (->> (for [diff [[-1 0] [0 -1] [1 0] [0 1]]]
             (mapv + diff pos))
           (remove seen)
           (remove (comp #{\#} (partial get-in grid)))
           (first)
           (conj path)
           (recur (conj seen pos))))))

(def path-len (count full-path))
(def distance-from-start (zipmap full-path (range path-len)))
(def distance-from-end   (zipmap full-path (reverse (range path-len))))

;; Seq of [r' [y' x']] pairs for points within distance r to [y x]
(defn neighbors-in-radius [r [y x]]
  (for [di (range (- r) (inc r))
        dj (range (- r) (inc r))
        :when (<= (+ (abs di) (abs dj)) r)]
    [[(+ y di) (+ x dj)] (+ (abs di) (abs dj))]))

(defn solve [r limit]
  (count (for [[ps c1] distance-from-start
               [n nr] (neighbors-in-radius r ps)
               :when (distance-from-end n)
               :let  [diff (- path-len 1 (+ c1 (distance-from-end n) nr))]
               :when (<= limit diff)]
           diff)))

(println 'First  (solve 2 100))
(println 'Second (solve 20 100))