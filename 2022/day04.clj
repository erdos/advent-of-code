(ns day04)

(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day03.clj"

(def sects
(for [line lines :let [[_ a b c d] (re-matches #"(\d+)\-(\d+),(\d+)\-(\d+)" line)]]
  [[(parse-long a) (parse-long b)] [(parse-long c) (parse-long d)]]))

(defn fully? [[a b] [c d]]
  (or (<= a c d b) (<= c a b d)))

(->> sects
     (filter (partial apply fully?))
     count
     (println))

(defn partially? [[a b] [c d]]
 (or (<= a c d b)
     (<= a c b d)
     (<= c a b d)
     (<= c a d b)))

(->> sects
     (filter (partial apply partially?))
     count
     (println))