(ns day09)

(load-file "common.clj")

(defn parse-line [line]
  (mapv parse-long (words line)))

(defn diffs [numbers]
  (map - (next numbers) numbers ))

(defn solve1 [numbers]
  (if (apply = numbers)
    (first numbers)
    (+ (last numbers) (solve1 (diffs numbers)))))

(->> lines
     (map parse-line)
     (map solve1)
     (reduce +)
     (println "First"))

(defn solve2 [numbers]
  (if (apply = numbers)
    (first numbers)
    (- (first numbers) (solve2 (diffs numbers)))))

(->> lines
     (map parse-line)
     (map solve2)
     (reduce +)
     (println "Second"))