(ns day22)

(defn evolve [n]
  (letfn [(mix [a b] (bit-xor a b))
          (prune [a] (mod a 16777216))]
    (as-> n n
      (mix n (* n 64)) (prune n)
      (mix n (quot n 32)) (prune n)
      (mix n (* n 2048)) (prune n))))

(def lines (map parse-long (clojure.string/split-lines (slurp *in*))))

(->> (transduce (map #(nth (iterate evolve %) 2000)) + lines)
     (time)
     (println 'First))

(defn line->map [line]
  (->> (iterate evolve line)
       (take 2001)
       (map #(mod % 10))
       (partition 2 1)
       (map (fn [[a b]] [(- b a) b]))
       (partition 4 1)
       (reduce (fn [m [[a] [b] [c] [d p]]]
                 (update m [a b c d] #(or %1 %2) p))
               {})))

(->> lines
     (map line->map)
     (apply merge-with +)
     (vals)
     (apply max)
     (time)
     (println 'Bananas))