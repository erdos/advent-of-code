(ns day14)

(def w 11) (def h 7)

(def robots ;; seq of [x y vx vy] tuples
  (->> (slurp *in*)
       (re-seq #"(-?\d+)")
       (map second)
       (map parse-long)
       (partition 4)))

(defn simulate-robot [steps [px py vx vy]]
  [(mod (+ px (* vx steps)) w)
   (mod (+ py (* vy steps)) h)
   vx vy])

(defn robot->quadrant [[px py]]
  (cond (= px (/ (dec w) 2)) nil
        (= py (/ (dec h) 2)) nil
        (< px (/ w 2))       (if (< py (/ h 2)) 1 3)
        (> px (/ w 2))       (if (< py (/ h 2)) 2 4)))

(assert (= [1 nil 2 3 4] (map robot->quadrant [[0 2] [5 2] [6 0] [1 4] [6 4]])))

(def w 101) (def h 103)

(->> robots
     (map (partial simulate-robot 100))
     (keep robot->quadrant)
     (frequencies)
     (vals)
     (reduce *)
     (println "First:"))

(defn to-map [robots]
  (reduce (fn [m [x y]]
            (assoc-in m [y x] \X))
          (vec (repeat h   (vec (repeat w \.))))
          robots))

(defn draw-map [map]
  (doseq [line map]
    (println (apply str line)))
  (println))

(->> robots
     (iterate (partial map (partial simulate-robot 1)))
     (map to-map)
     (keep-indexed
      (fn [idx m]
        (when (some (fn [line]
                      (.contains (apply str line) "XXXXXXXX")) m)
          (println 'Second idx) m)))
     (first)
     (draw-map))
