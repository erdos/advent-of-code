#!/usr/bin/env bb

(set! *warn-on-reflection* true)

(require '[clojure.string :as s])

(defn ->long [x] (Long/valueOf ^String x))
(defn third [x] (nth x 2))

(def items (for [[c & xs]
                 (partition 7 (re-seq #"on|off|-\d+|\d+" (slurp "input22.txt")))]
             [(keyword c) (mapv vec (partition 2 (map ->long xs)))]))

(defn points [[[a b] [c d] [e f]]]
  (for [i (range (max a -50) (inc (min 50 b)))
        j (range (max c -50) (inc (min 50 d)))
        k (range (max e -50) (inc (min 50 f)))]
    [i j k]))

(->> items
     (reduce (fn [s [cmd region]]
               ; (println :state-volume (count s))
               (case cmd
                 :on (apply conj s (points region))
                 :off (apply disj s (points region))))
             #{})
     (count)
     (println "First answer:"))

;; SECOND PART

(defn realpoints [& triples]
  (filter (fn [[cmd a b]] (<= a b)) triples))

(defn overlap-segment [[a b :as a+b] [c d :as c+d]]
  {:pre [(<= a b) (<= c d)]
   :post [(every? true? (for [[_ x y] %] (<= x y)))]}
  (cond

    ;; no intersection
    (or (< b c) (< d a)) (list (cons :first a+b) (cons :second c+d))

    ;; partial overlap
    (<= a c b d) (realpoints [:first a (dec c)] [:both c b] [:second (inc b) d])
    (<= c a d b) (realpoints [:second c (dec a)] [:both a d] [:first (inc d) b])

    ;; one contains another
    (<= a c d b) (realpoints [:first a (dec c)] (cons :both c+d) [:first (inc d) b])
    (<= c a b d) (realpoints [:second c (dec a)] (cons :both a+b) [:second (inc b) d])

    :else (assert false (str "Unexpected coords " (pr-str [[a b] [c d]])))))

(defn same-code? [a b c] (->> [a b c] (remove #{:both}) ((fn [x] (or (empty? x) (apply = x))))))
(def same-code (memoize same-code?))

(defn subtract-cubes [xs ys]
  (let [[a b c] (map overlap-segment xs ys)]
    (for [[xcode x1 x2] a
          [ycode y1 y2] b
          [zcode z1 z2] c
          :when (and (or (= :first xcode) (= :first ycode) (= :first zcode))
                     (and (not= :second xcode) (not= :second ycode) (not= :second zcode)))]
      [[x1 x2] [y1 y2] [z1 z2]])))


(assert (empty? (subtract-cubes [[4 5] [4 5] [4 5]] [[4 5] [4 5] [4 5]]))) ;; subtract from itself
(assert (empty? (subtract-cubes [[4 5] [4 5] [4 5]] [[1 10] [1 10] [1 10]]))) ;; total coverage
(assert (= 7 (count (subtract-cubes [[4 5] [4 5] [4 5]] [[4 4] [4 4] [4 4]])))) ;; just removes one pt from the corner

;; hole in the middle
(assert (= 26 (count (subtract-cubes [[1 10] [1 10] [1 10]] [[4 5] [4 5] [4 5]]))))

(defn cube-volume [sides] (reduce * 1N (map (fn [[a b]] (inc (- b a))) sides)))

;; if one cube ends with z same as next then two can be merged
(defn- merge-neighbors [sorted-cubes index]
  ((fn f [last-cube cubes]
     (assert (vector? last-cube) (str "Not vec " (pr-str last-cube)))
     (if-let [[c & cs] (seq cubes)]
       (if (= (inc (get-in last-cube [index 1])) (get-in c [index 0])) ;; same z
         (let [merged-cube (assoc-in last-cube [index 1] (get-in c [index 1]))]
           (recur merged-cube cs))
         (cons last-cube (lazy-seq (f c cs))))
       [last-cube]))
   (first sorted-cubes) (next sorted-cubes)))

;; return a seq of cubes where some are merged
(defn simplify-shards-z [cubes]
  (for [g (vals (group-by (juxt first second) cubes))
        :let [sorted-cubes (sort-by (fn [[_ _ [zmin zmax]]] zmax) g)]
        e (merge-neighbors sorted-cubes 2)]
    e))

(defn simplify-shards-y [cubes]
  (for [g (vals (group-by (juxt first third) cubes))
        :let [sorted-cubes (sort-by (fn [[_ [ymin ymax] _]] ymax) g)]
        e (merge-neighbors sorted-cubes 1)]
    e))

(defn simplify-shards-x [cubes]
  (for [g (vals (group-by (juxt second third) cubes))
        :let [sorted-cubes (sort-by (fn [[[xmin xmax] _ _]] xmax) g)]
        e (merge-neighbors sorted-cubes 0)]
    e))

(defn simplify-shards [cubes]
  (-> cubes simplify-shards-z simplify-shards-y simplify-shards-x))

(defn fixpt [f x] (let [fx (f x)] (if (= fx x) fx (recur f fx))))

(def simplify-shards-fix (partial fixpt simplify-shards))
(def subtract-cubes (comp simplify-shards-fix subtract-cubes))

(defn pmapcat [f xs] (apply concat (pmap f xs)))

(defn add-to-cubes [cubes new-cube]
  (let [new-shards
        ;; remove existing pts from new cube
        (reduce (fn [new-shards old-shard]
                  (doall (pmapcat (fn [nss] (subtract-cubes nss old-shard)) new-shards)))
                [new-cube] cubes)]
    (into cubes (doall new-shards))))

(->> items
     (reduce (fn [s [cmd new-cube]]
               (->>
                (case cmd
                  :on  (add-to-cubes s new-cube)
                  :off (set (pmapcat (fn [oldcube] (subtract-cubes oldcube new-cube)) s)))
                (fixpt simplify-shards)))
             #{})
     (map cube-volume)
     (reduce +)
     (println "Second answer"))

(shutdown-agents)
