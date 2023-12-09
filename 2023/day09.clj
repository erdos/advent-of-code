(ns day09)

(load-file "common.clj")

(defn parse-line [line]
  (mapv parse-long (.split line " ")))

(defn diffs [numbers]
  (map - (next numbers) numbers))

(defn solve [numbers]
  (if (apply = numbers)
    (first numbers)
    (+ (last numbers) (solve (diffs numbers)))))

#_ ;; alternatively, with reduce:
(defn solve [numbers]
  (reduce (fn [sum numbers] (+ sum (last numbers))) 
          0 (take-while (partial apply not= 0) (iterate diffs numbers))))

#_ ;; alternatively, with loop-recur:
(defn solve [numbers]
  (loop [numbers numbers
         acc    0]
    (if (apply = numbers)
      (+ (first numbers) acc)
      (recur (diffs numbers) (+ acc (last numbers))))))

(->> lines
     (map parse-line)
     (map solve)
     (reduce +)
     (println "First"))

(->> lines
     (map parse-line)
     (map reverse)
     (map solve)
     (reduce +)
     (println "Second"))