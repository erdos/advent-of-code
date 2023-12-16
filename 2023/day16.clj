(ns day16)

(load-file "common.clj")

(defn step [[y x dir]]
  (case (get-in grid [y x])
    \. (case dir :right (list [y (inc x) :right])
                 :left  (list [y (dec x) :left])
                 :up    (list [(dec y) x :up])
                 :down  (list [(inc y) x :down]))
    \| (case dir (:left :right) (list [(dec y) x :up] [(inc y) x :down])
                 :up            (list [(dec y) x :up])
                 :down          (list [(inc y) x :down]))
    \- (case dir (:up :down) (list [y (dec x) :left] [y (inc x) :right])
                 :left       (list [y (dec x) :left])
                 :right      (list [y (inc x) :right]))
    \\ (case dir :left  (list [(dec y) x :up])
                 :right (list [(inc y) x :down])
                 :up    (list [y (dec x) :left])
                 :down  (list [y (inc x) :right]))
    \/ (case dir :left  (list [(inc y) x :down])
                 :right (list [(dec y) x :up])
                 :up    (list [y (inc x) :right])
                 :down  (list [y (dec x) :left]))))

(defn in-bounds [[y x :as pos]]
  (when (and (< -1 y (count grid)) (< -1 x (count (grid y)))) pos))

(defn count-energized [starting]
  (assert (in-bounds starting))
  (loop [past-active (sorted-set)
         active      #{starting}]
  (if (seq active)
    (recur (into past-active active)
           (->> active (mapcat step) (keep in-bounds) (remove past-active)))
    (->> past-active (map (partial take 2)) set count))))

(println "First" (count-energized [0 0 :right]))

(->> (concat (for [y (range (count grid))] [y 0 :right])
             (for [y (range (count grid))] [y (dec (count (grid y))) :left])
             (for [x (range (count (grid 0)))] [0 x :down])
             (for [x (range (count (grid 0)))] [(dec (count grid)) x :up]))
     (map count-energized)
     (apply max)
     (println "Second"))
