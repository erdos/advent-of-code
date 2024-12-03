(ns day03)

(->> (for [[_ a b] (re-seq #"mul\((\d+),(\d+)\)" (slurp "data.txt"))]
       (* (parse-long a) (parse-long b)))
     (reduce +)
     (println "First:"))

(->> (slurp "data.txt")
     (re-seq #"((mul)\((\d+),(\d+)\)|(don't)\(\)|(do)\(\))")
     (reduce (fn [[enabled? sum] [_ x y a b]]
               (cond
                 (= x "don't()") [false sum]
                 (= x "do()")    [true sum]
                 (= y "mul")     (if enabled?
                                   [true (+ sum (* (parse-long a) (parse-long b)))]
                                   [false sum])))
             [true 0])
     (second)
     (println "Second:"))