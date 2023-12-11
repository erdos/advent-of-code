(ns day10)

(load-file "common.clj")

(defn parse-line [line]
  (mapv parse-long (.split line " ")))

(def empty-rows
  (into (sorted-set)
        (for [[i row] (map-indexed vector grid)
              :when (every? #{\.} row)]
          i)))

(def empty-cols
  (into (sorted-set)
        (for [j (range (count (first grid)))
              :when (every? #{\.} (for [[i row] (map-indexed vector grid)]
                                    (get-in grid [i j])))]
           j)))

(def galaxies
  (for [[i row] (map-indexed vector grid)
        [j cell] (map-indexed vector row)
        :when (= \# cell)]
    [i j]))

(defn manhattan2 [[a b :as one] [x y :as two] scale]
  (let [aa (count (subseq empty-rows > (min a x) < (max a x)))
        bb (count (subseq empty-cols > (min b y) < (max b y)))]
     (+ (- (abs (- a x)) aa)
        (* scale aa)
        (- (abs (- b y)) bb)
        (* scale bb))))

(->> (for [a galaxies, b galaxies] (manhattan2 a b 2))
     (reduce +)
     (* 1/2) (long)
     (println "First")
     (time))

(->> (for [a galaxies, b galaxies] (manhattan2 a b 1000000))
     (reduce +)
     (* 1/2) (long)
     (println "Second")
     (time))