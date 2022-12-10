(ns day08)
(declare data lines)
(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day09.clj"

;(def config (butlast (take-while not-empty lines)))
;(def config (for [c config] (vec (take-nth 4 (next c) ))))

;(def commands
;  (for [line (next (drop-while not-empty lines))
;        :let [[_ cnt from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]]
;    [(parse-long cnt) (dec (parse-long from)) (dec (parse-long to))]))

(def cmds
  (for [[d _ & len] lines]
    [(keyword (str d)) (parse-long (apply str len))]))

(defn move [[hx hy] dir]
  (case dir
    :U [hx (inc hy)]
    :D [hx (dec hy)]
    :L [(dec hx) hy]
    :R [(inc hx) hy]))

(defn follow [[tx ty] [hx hy]]
  (if (and (#{0 1} (Math/abs (- tx hx)))
           (#{0 1} (Math/abs (- ty hy))))
    [tx ty]
    [(- tx (compare tx hx)) (- ty (compare ty hy))]))

(->>
 cmds
 (mapcat (fn [[d len]] (repeat len d)))
 (reductions move [0 0]) ;; head pos

 (reductions follow [0 0])
 (reductions follow [0 0])

 (reductions follow [0 0])
 (reductions follow [0 0])
 
 (reductions follow [0 0])
 (reductions follow [0 0])

 (reductions follow [0 0])
 (reductions follow [0 0])

 (reductions follow [0 0])

 distinct
 count)

#_(->> data )