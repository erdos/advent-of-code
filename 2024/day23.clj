(ns day23)

(def input (->> (slurp *in*)
                (clojure.string/split-lines)
                (map #(vec (.split % "-")))))

(def connections
  (apply merge-with into (for [[a b] input] {a #{b} b #{a}})))

(defn chief? [^String name] (.startsWith name "t"))

(->> (for [[a b] input
           c (filter (connections a) (connections b))]
       #{a b c})
     (distinct)
     (filter (partial some chief?))
     (count)
     (time)
     (println 'First))

(defn extend-set [s]
  (let [pivot (first s)]
    (sequence (comp (mapcat connections)
                    (distinct)
                    (filter (comp pos? (partial compare pivot)))
                    (filter #(every? (connections %) s))
                    (map (partial conj s)))
              s)))

(->> (keys connections)
     (map sorted-set)
     (iterate (partial mapcat extend-set))
     (take-while seq)
     (last)
     (first)
     (clojure.string/join ",")
     (time)
     (println 'Second))
