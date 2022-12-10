(ns day05)

(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day03.clj"

(->> (butlast (take-while not-empty lines))
     (map #(read-string (str "(" (str/replace % #"   [ \n]" "[,],") ")")))
     (transpose)
     (mapv (partial apply concat))
     (def stacks))

(def commands
  (for [line (next (drop-while not-empty lines))
        :let [[_ cnt from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]]
    [(parse-long cnt) (dec (parse-long from)) (dec (parse-long to))]))

(->> commands
  (reduce
    (fn [stacks [cnt from to]]
        (-> stacks
            (update from (partial drop cnt))
            (update to into (reverse (take cnt (stacks from)))))) ;; remove reverse for part 1
    stacks)
  (map first)
  (apply str))
