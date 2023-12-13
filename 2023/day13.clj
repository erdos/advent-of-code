(ns day13)

(load-file "common.clj")

(def blocks (map str/split-lines (.split data "\\n\\n")))

(def mirror=  =)

(defn solve-hor [block]
  (loop [lines (next block)
         stack (list (first block))]
    (if (seq lines)
      (if (mirror= (take (count lines) stack) (take (count stack) lines))
        (count stack)
        (recur (next lines) (conj stack (first lines)))))))

(def solve-ver (comp solve-hor transpose))

(defn solve [block]
  (or (some-> (solve-hor block) (* 100))
      (solve-ver block)))

(println "First" (transduce (map solve) + blocks))

(defn mirror= [a b]
  (= 1 (reduce + (mapcat (partial map #(if (= %1 %2) 0 1)) a b))))

(println "Second" (transduce (map solve) + blocks))
