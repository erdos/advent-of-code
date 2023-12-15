(ns day14)

(def data (-> *in* (slurp) (.replaceAll "\n+" "") (.split ",")))

(defn hh [label]
  (reduce (fn [v c] (-> c int (+ v) (* 17) (rem 256))) 0 label))

(println "First" (transduce (map hh) + data))

(def steps
  (for [d data]
    (let [[a b] (.split d "[=-]")]
      [(if b assoc dissoc) a (some-> b parse-long)])))

(defn focusing-power [box]
  (reduce + (map * (next (range)) (vals box))))

(defn sum-boxes [boxes]
  (reduce-kv (fn [sum i box] (+ sum (* (inc i) (focusing-power box)))) 0 boxes))

(->> steps
     (reduce (fn [boxes [cmd label value]] (update boxes (hh label) cmd label value))
             (vec (repeat 256 (array-map))))
     (sum-boxes)
     (println "Second"))
