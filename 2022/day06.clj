(ns day06)

(declare data lines)

(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day03.clj"

;(def config (butlast (take-while not-empty lines)))
;(def config (for [c config] (vec (take-nth 4 (next c) ))))

;(def commands
;  (for [line (next (drop-while not-empty lines))
;        :let [[_ cnt from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]]
;    [(parse-long cnt) (dec (parse-long from)) (dec (parse-long to))]))

(defn solve [n data]
  (->> data
    (partition n 1)
    (map-indexed (fn [idx item] (when (= n (count (set item))) (+ n idx))))
    (filter number?)
    first))

(println (solve 4 data))
(println (solve 14 data))
