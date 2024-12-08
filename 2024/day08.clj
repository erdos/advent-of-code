(ns day08)

(->> (slurp *in*)
     (clojure.string/split-lines)
     (mapv vec)
     (def grid))

(-> (for [i (range (count grid))
          j (range (count (grid i)))]
      [i j])
    (->> (group-by (partial get-in grid)))
    (dissoc \.)
    (vals)
    (->> (def locations)))

(defn in-bounds? [[i j]]
  (and (< -1 i (count grid)) (< -1 j (count (grid 0)))))

(defn pairs [xs]
  (for [[t & ts] (iterate next xs) :while t, u ts]
    [t u]))

(defn cross [xs]
  (for [a xs b xs] [a b]))

(defn vec+ [a b] (mapv + a b))
(defn vec- [a b] (mapv - a b))

(defn mirrors [a b]
  [(vec+ b (vec- b a)) (vec- a (vec- b a))])

(def counter ((map (constantly 1)) +))

(->> (transduce (comp (mapcat pairs)
                      (mapcat (partial apply mirrors))
                      (filter in-bounds?)
                      (distinct))
                counter locations)
     (println 'First)
     (time))

(defn harmonics [pt1 pt2]
  (iterate (partial vec+ (vec- pt1 pt2)) pt1))

(->> (transduce (comp (mapcat cross)
                      (remove (partial apply =))
                      (mapcat (comp (partial take-while in-bounds?)
                                    (partial apply harmonics)))
                      (distinct))
                counter locations)
     (println 'Second)
     (time))
