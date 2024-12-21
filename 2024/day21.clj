(ns day21)

(defn code-numeric [line]
  (->> line
       (filter (set "0123456789"))
       (drop-while #{\0})
       (apply str)
       (parse-long)))

(def directional-mapping (zipmap "^A<v>" [[0 -1] [0 0] [1 -2] [1 -1] [1 0]]))
(def numeric-mapping (zipmap "789456123 0A"
                             [[-3 -2] [-3 -1] [-3 0]
                              [-2 -2] [-2 -1] [-2 0]
                              [-1 -2] [-1 -1] [-1 0]
                              '[____] [0  -1] [0  0]]))

(def button-mapping (merge directional-mapping numeric-mapping))

;; seq of sequences that gets us from here to there
(defn sequences-step [[y1 x1] [y2 x2]]
  (cond-> []
    (> x1 x2) (conj \<)
    (> y1 y2) (conj \^)
    (< x1 x2) (conj \>)
    (< y1 y2) (conj \v)))

(defn pos-step [[y x] c]
  (case c
    \> [y (inc x)]
    \< [y (dec x)]
    \v [(inc y) x]
    \^ [(dec y) x]))

;; sequences to get from pos1 to pos2
(defn loc-sequences [start end]
  (assert (vector? start) (str "1 not vec: " (pr-str start)))
  (assert (vector? end) (str "2 Not vec " (pr-str end)))
  (cond (= start [0 -2]) []
        (= start end) [[]]
        :else (for [step (sequences-step start end)
                    :let [mid (pos-step start step)]
                    tail (if (= mid end) [[]] (loc-sequences mid end))]
                (cons step tail))))

(defn map-pairs [f s] (map f s (next s)))

(defn solve [n code]
  (if (zero? n)
    (count code)
    (->> code
         (map button-mapping)
         (cons [0 0])
         (map-pairs (fn [a b] (for [s (loc-sequences a b)]
                                (solve (dec n) (concat s [\A])))))
         (map (partial apply min))
         (reduce +))))

(def solve (memoize solve))

(let [lines (clojure.string/split-lines (slurp *in*))]
  (doseq [robot-cnt [2 25]]
    (->> lines
         (transduce (map #(* (code-numeric %) (solve (inc robot-cnt) %))) +)
         (println 'Answer))))