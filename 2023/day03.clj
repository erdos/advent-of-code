(ns day03)

(load-file "common.clj")

(defn consuming [f accumulator sequence]
  (when (seq sequence)
    (when-let [[v accumulator tail] (f accumulator sequence)]
      (cons v (lazy-seq (consuming f accumulator tail))))))

;; seq of [i j "value"] items
(def all-items
  (apply concat
         (map-indexed
          (fn [i line]
            (consuming (fn [offset line]
                         (when (seq line)
                           (if (digits (first line))
                             (let [[head tail] (split-with digits line)]
                               [[i offset head] (+ offset (count head)) tail])
                             (recur (inc offset) (next line)))))
                       0 line)) lines)))

(defn item-neighs [[i j value]]
   (for [di [-1 0 +1]
         dj (if (zero? di)
              [-1 (count value)]
              (range -1 (inc (count value))))]
     (get-in grid [(+ i di) (+ j dj)])))

(->> all-items
     (keep (fn [[i j value]]
             (let [all-n (remove nil? (item-neighs [i j value]))]
               (when (seq (remove (set "0123456789.") all-n))
                 (parse-long (apply str value))))))
     (reduce +) (println "First:"))

;; coordinates of all neighbors that are * symbols
(defn gear-neighbors [[i j value]]
  (for [di [-1 0 +1]
        dj (if (zero? di)
             [-1 (count value)]
             (range -1 (inc (count value))))
        :when (= \* (get-in grid [(+ i di) (+ j dj)]))]
    [(+ i di) (+ j dj)]))

(->> (reduce (fn [m [i j v]]
               (reduce (fn [m neighbor] (update m neighbor conj v))
                       m (gear-neighbors [i j v])))
             {} all-items)
     (vals)
     (filter #(= 2 (count %)))
     (map (fn [[as bs]] (* (parse-long (apply str as)) (parse-long (apply str bs)))))
     (reduce +)
     (println "Second:"))

; (println grid)
;; all neighs:
