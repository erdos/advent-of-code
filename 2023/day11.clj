(ns day10)

(load-file "common.clj")

(def empty-rows
  (into (sorted-set)
        (keep-indexed (fn [i row] (when (every? #{\.} row) i)))
        grid))

(def empty-cols
  (into (sorted-set)
        (keep-indexed (fn [j col] (when (every? #{\.} col) j)))
        (transpose grid)))

(def galaxies
  (for [[i row] (map-indexed vector grid)
        [j cell] (map-indexed vector row)
        :when (= \# cell)]
    [i j]))

(defn manhattan2 [[a b :as one] [x y :as two] scale]
  (let [aa (count (subseq empty-rows > (min a x) < (max a x)))
        bb (count (subseq empty-cols > (min b y) < (max b y)))]
     (+ (abs (- a x))
        (abs (- b y))
        (* (dec scale) (+ aa bb)))))

(defn solve [label scale]
  (->> (for [a galaxies, b galaxies] (manhattan2 a b scale))
       (reduce +)
       (* 1/2) (long)
       (println label)
       (time)))

(solve "First" 2)
(solve "First" 1000000)