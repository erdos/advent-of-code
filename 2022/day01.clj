(ns day01)

(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day01.clj"

(def lines (map #(safely (parse-long %)) lines))
(def lines (keep (partial reduce +) (partition-by nil? lines)))

(println "First:" (apply max lines))
(println "Second:" (apply + (take-last 3 (sort lines))))
