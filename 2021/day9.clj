#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn ->long [s] (Long/parseLong (str s)))

(def area (mapv
           (partial mapv ->long)
           (s/split-lines (slurp "input9.txt"))))

(defn height [[x y]] (get-in area [x y] 999))
(defn risk-level [[x y]] (inc (height [x y])))

(defn lowpoints []
  (for [i (range (count area))
        j (range (count (first area)))
        :let [x (height [i j])
              a (height [(dec i) j])
              b (height [(inc i) j])
              c (height [i (dec j)])
              d (height [i (inc j)])]
        :when (< x (min a b c d))]
    [i j]))

(->> (reduce + (map risk-level (lowpoints)))
     (println "First answer:"))

(defn all-coords []
  (for [i (range (count area))
        j (range (count (first area)))]
    [i j (height [i j])]))

(defn basin? [[x y]]
  (< (height [x y]) 9))

(def ccount (atom  0))

(defn ccreducer
  ([coord->cluster [x y]]
   (ccreducer coord->cluster [x y] (swap! ccount inc)))
  ([coord->cluster [x y] idx]
   (if (coord->cluster [x y])
     coord->cluster ;; already visited
     (cond-> (assoc coord->cluster [x y] idx)
       (basin? [(dec x) y]) (ccreducer [(dec x) y] idx)
       (basin? [(inc x) y]) (ccreducer [(inc x) y] idx)
       (basin? [x (dec y)]) (ccreducer [x (dec y)] idx)
       (basin? [x (inc y)]) (ccreducer [x (inc y)] idx)))))

(def coord->cluster (reduce ccreducer {}
                            (filter basin? (all-coords))))

(def cluster-sizes
  (reduce (fn [acc idx]
            (update acc idx (fnil inc 0))) {} (vals coord->cluster)))


(assert (= (reduce + (vals cluster-sizes)) (count (filter basin? (all-coords)))))

(->> (reduce * (take 3 (sort-by - (vals cluster-sizes))))
     (println "Second answer:"))
