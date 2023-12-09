(ns day09)

(load-file "common.clj")

(defn parse-line [line]
  (mapv parse-long (.split line " ")))

(defn diffs [numbers]
  (map - (next numbers) numbers))

(defn solve1 [numbers]
  (if (apply = numbers)
    (first numbers)
    (+ (last numbers) (solve1 (diffs numbers)))))

#_ ;; alternatively, with reduce:
(defn solve1 [numbers]
  (reduce (fn [sum numbers] (+ sum (last numbers))) 
          0 (take-while (partial apply not= 0) (iterate diffs numbers))))

#_ ;; alternatively, with loop-recur:
(defn solve1 [numbers]
  (loop [numbers numbers
         acc    0]
    (if (apply = numbers)
      (+ (first numbers) acc)
      (recur (diffs numbers) (+ acc (last numbers))))))

(->> lines
     (map parse-line)
     (map solve1)
     (reduce +)
     (println "First"))

;; second part is the same really, only we subtract the diff from the first nr
(defn solve2 [numbers]
  (if (apply = numbers)
    (first numbers)
    (- (first numbers) (solve2 (diffs numbers)))))

(->> lines
     (map parse-line)
     (map solve2)
     (reduce +)
     (println "Second"))