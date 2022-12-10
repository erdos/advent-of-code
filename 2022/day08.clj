(ns day08)

(declare data lines)

(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day08.clj"

;(def config (butlast (take-while not-empty lines)))
;(def config (for [c config] (vec (take-nth 4 (next c) ))))

;(def commands
;  (for [line (next (drop-while not-empty lines))
;        :let [[_ cnt from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]]
;    [(parse-long cnt) (dec (parse-long from)) (dec (parse-long to))]))

(def grid (mapv (partial mapv (comp parse-long str)) lines))

(def grid*
  (for [[y line] (map-indexed vector lines)]
    (for [[x c] (map-indexed vector line)]
      {:id (str x '- y)
       :height (parse-long (str c))})))

;(println grid*)
;; returns seq of tree indices
(defn vis-row [row]
  (assert (sequential? row))
  (assert (every? map? row) (pr-str row))
  (second (reduce (fn [[max cnt] tree]
                    (if (> (:height tree) (:height max))
                      [tree (cons tree cnt)]
                      [max cnt]))
                  [{:height -1} ()]
                  row)))

(->>
 (concat (mapcat vis-row grid*)
         (mapcat (comp vis-row reverse) grid*)
         (mapcat vis-row (transpose grid*))
         (mapcat (comp vis-row reverse) (transpose grid*)))
 set count (println "First: "))

(defn map-scenic-left [row]
  (map (fn [item suffix]
         (let [[a b] (split-with (fn [s] (< s item)) suffix)]
           (+ (count (take-while (fn [s] (< s item)) suffix))
              (if (seq b) 1 0))))
       row
       (concat (next (suffixes row)))))

(defn assoc-scenic-line [row key]
  (map (fn [c s] (assoc c key s))
       row
       (map-scenic-left (map :height row))))

(defn assoc-scenic-left [grid* key]
  (for [line grid*]
    (assoc-scenic-line line key)))

(defn assoc-scenic-row [grid* lk rk]
  (-> grid*
      (assoc-scenic-left lk)
      (->> (map reverse))
      (assoc-scenic-left rk)
      (->> (map reverse))))

(defn assoc-scenic [grid*]
  (-> grid*
      (assoc-scenic-row :left :right)
      transpose
      (assoc-scenic-row :top :bottom)
      transpose))

(->> grid*
     (assoc-scenic)
     flatten
     (map (juxt :top :left :bottom :right))
     (map (partial apply *))
     (apply max)
     (println "Second:"))
