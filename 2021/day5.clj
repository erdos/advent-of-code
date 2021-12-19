(require '[clojure.string :as s])

(defn ->long [s] (Long/parseLong s))

(def endpoints
  (for [line (vec (s/split-lines (slurp "input5.txt")))
        :let [[_ a b c d] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" line)]]
    [[(->long a) (->long b)] [(->long c) (->long d)]]))

(def size (inc (apply max (flatten endpoints))))
(def grid (vec (repeat size (vec (repeat size 0)))))

(defn ringo [a b] (if (< a b) (range a (inc b)) (reverse (range b (inc a)))))

(assert (= [4] (ringo 4 4)))
(assert (= [3 2 1] (ringo 3 1)))
(assert (= [1 2 3] (ringo 1 3)))
(assert (= [3 4 5] (ringo 3 5)))

(defn pts [[a b] [c d]]
  (cond
    (= a c) (for [i (ringo b d)] [a i])
    (= b d) (for [i (ringo a c)] [i b])
    :else   []))

(->>
 (reduce (fn [grid coords] (update-in grid coords inc))
         grid
         (mapcat (partial apply pts) endpoints))
 (flatten)
 (filter (partial <= 2))
 (count)
 (println "First answer"))

(defn pts [[a b] [c d]]
  (cond
    (= a c) (for [i (ringo b d)] [a i])
    (= b d) (for [i (ringo a c)] [i b])
    :else   (doall (map vector (ringo a c) (ringo b d)))))

(assert (= [[1 1] [2 2] [3 3]] (pts [1 1] [3 3])))
(assert (= [[3 3] [2 2] [1 1]] (pts [3 3] [1 1])))
(assert (= [[9 7] [8 8] [7 9]] (pts [9 7] [7 9])))
(assert (= [[1 3] [1 4] [1 5]] (pts [1 3] [1 5])))

(->>
 (reduce (fn [grid coords] (update-in grid coords inc))
         grid
         (mapcat (partial apply pts) endpoints))
 (flatten)
 (filter (partial <= 2))
 (count)
 (println "Second answer:"))
