(ns day02)

(load-file "common.clj")

(defn summa [colors]
  (reduce (fn [m [color n]] (update m color (fnil + 0) n)) {} colors))

(defn possible? [id colors]
  (every? (fn [set]
            (let [m (summa set)]
              (and (<= (m "red" 0) 12)
                   (<= (m "green" 0) 13)
                   (<= (m "blue" 0) 14))))
          colors))

(->>
 (for [line lines
       :let [id (-> line (str/split #": ") first (str/split #" ")
                    second parse-long)
             sets (-> line (str/split #": ") second
                      (str/split #"; ") vec)
             colors
             (for [s sets]
               (-> s (str/split #", ")
                   (->> (mapv (fn [item]
                                (let [[k v] (vec (str/split item #" "))]
                                  [v (parse-long k)]))))))]
       :when (possible? id colors)]
   id)
 (sum)
 (println "First"))

(defn fewest [colors]
  (->> (map summa colors)
       (apply merge-with max)
       (vals)
       (reduce *)))

(->>
 (for [line lines
       :let [colors
             (for [s (-> line (str/split #": ") second (str/split #"; ") vec)]
               (mapv (fn [item]
                       (let [[k v] (vec (str/split item #" "))]
                         [v (parse-long k)]))
                     (str/split s #", ")))]]
   (fewest colors))
 (sum)
 (println "Second"))

;; not 79
