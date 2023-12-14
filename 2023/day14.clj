(ns day14)

(load-file "common.clj")

(defn tilt-row [line]
  (when (seq line)
    (if (and (= \. (first line)) (= \O (second line)))
      (list* \O \. (tilt-row (nnext line)))
      (cons (first line) (tilt-row (next line))))))

(defn tilt [grid]
  (->> grid
       transpose
       (mapv (partial fixpt tilt-row))
       transpose))

(defn rot-ccw [grid] (mapv reverse (transpose grid)))

(def spin-cycle (comp tilt rot-ccw tilt rot-ccw tilt rot-ccw tilt rot-ccw))

(defn calc-load [grid]
  (->> (map #(count (filter #{\O} %)) grid)
       (reverse)
       (cons 0) (map-indexed *)
       (reduce +)))

(->> grid tilt calc-load
     (println "First") (time))

(defn nth-repeat [f n state]
  (loop [state state
         i 0
         i->state {}
         state->i {}]
    (cond (= n i)    state
          (contains? state->i state)
          (let [last-i (state->i state)]
            (i->state (+ last-i (rem (- n i) (- i last-i)))))
          :else
          (recur (f state) (inc i)
                 (assoc i->state i state)
                 (assoc state->i state i)))))

(->> grid
     (nth-repeat spin-cycle 1000000000)
     calc-load
     (println "Second") (time))