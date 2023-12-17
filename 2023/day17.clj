(ns day17)

(require '[loom.alg-generic :refer [dijkstra-traverse]])

(->> *in* slurp (clojure.string/split-lines)
     (mapv (partial mapv (comp parse-long str)))
     (def grid))

(def left {:up :left :left :down :down :right :right :up})
(def right {:up :right :right :down :down :left :left :up})

;; returns tuple of next dir and dirhist
(defn next-dirs [dir dirhist]
  (if (= 2 dirhist)
    (list [(left dir) 0] [(right dir) 0])
    (list [(left dir) 0] [(right dir) 0] [dir (inc dirhist)])))

(defn next-pos [[y x] dir]
  (case dir :up    [(dec y) x]
            :down  [(inc y) x]
            :left  [y (dec x)]
            :right [y (inc x)]))

(def bottom-right [(dec (count grid)) (dec (count (grid 0)))])

(defn successors [[y1 x1 past-dir past-dir-hist]]
  (for [[dir dirhist] (next-dirs past-dir past-dir-hist)
        :let [[y x] (next-pos [y1 x1] dir)]
        :when (< -1 y (count grid))
        :when (< -1 x (count (grid 0)))]
    (if (= bottom-right [y x])
      bottom-right
      [y x dir dirhist])))

(defn dist [_ [y x]] (get-in grid [y x]))
(defn f [node state] state)

(->> (loom.alg-generic/dijkstra-traverse successors dist [0 0 :right 0] f)
     (some (fn [m] (m bottom-right)))
     (first)
     (println "First:")
     (time))

;; -------

(defn next-dirs [dir h]
  (case h
        (0 1 2) (list [dir (inc h)])
        (9)     (list [(left dir) 0] [(right dir) 0])
                (list [(left dir) 0] [(right dir) 0] [dir (inc h)])))

(defn successors2 [[_ _ _ h :as args]]
  (cond->> (successors args)
    (< 3 h 10) (remove #{bottom-right})))

(->> (loom.alg-generic/dijkstra-traverse successors2 dist [0 0 :right 0] f)
     (some (fn [m] (m bottom-right)))
     (first)
     (println "Second:")
     (time))
