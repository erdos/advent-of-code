#!/usr/bin/env bb

(defn ->long [s] (Long/parseLong s))

(def input (slurp "input19.txt"))

(def lines (vec (clojure.string/split-lines input)))

(def scanners ;; name->pts
  (loop [tail (next lines)
         scan-idx (first lines)
         output {}
         scan-pts #{}]
    (if-let [[line & tail] (seq tail)]
      (cond (.startsWith line "--")
            (recur tail line output #{})

            (.isEmpty line)
            (recur tail nil (assoc output scan-idx scan-pts) #{})

            :else
            (let [line (mapv ->long (.split line ","))]
              (recur tail scan-idx output (conj scan-pts line))))
      (assoc output scan-idx scan-pts))))

(defn pow2 [x] (Math/pow x 2))
(defn dist [[a b c] [d e f]] (+ (pow2 (- a d)) (pow2 (- b e)) (pow2 (- c f))))
(defn manhattan [[a b c] [d e f]] (+ (Math/abs (- a d)) (Math/abs (- b e)) (Math/abs (- c f))))

(defn camrotations [- [x y z]]
  (for [[x y z] [[x y z] [y (- x) z] [(- x) (- y) z] [(- y) x z]]  ;; around z
        [x y z] [[x y z] [z y (- x)] [(- x) y (- z)] [(- z) y x]]  ;; around y
        [x y z] [[x y z] [x z (- y)] [x (- y) (- z)] [x (- z) y]]] ;; around x
    [x y z]))

(def camrotators (map (partial apply juxt) (camrotations (partial comp -) [first second last])))

(defn translate [[a b c] [x y z]] [(- x a) (- y b) (- z c)])

;; returns a function that maps points from k2 into k1
(defn try-connect-fn [k1 k2]
  (let [points1 (get scanners k1)
        points2 (get scanners k2)]
    (first
     (for [pivot1 points1
           :let [pts1 (for [p points1] (translate pivot1 p))
                 pts1s (set pts1)]
           rotator camrotators
           pivot2 points2
           :let [transformer (fn [p] (rotator (translate pivot2 p)))]
           :let [common-pts (filter pts1s (map transformer points2))]
           :when (<= 12 (count common-pts))]
       (comp (fn [[x y z]] (translate [(- x) (- y) (- z)] pivot1)) transformer)))))
(def try-connect-fn (memoize try-connect-fn))

(let [dist-signature (memoize (fn [k] (set (for [a (scanners k) b (scanners k)] (dist a b)))))]
  (defn potential?  [k1 k2]
    (< 30 (count (filter (dist-signature k1) (dist-signature k2)))))
  (def potential? (memoize potential?)))

(def potential-children
  (->> (for [[a] scanners [b] scanners :when (not= a b)] [a b])
       (reduce (fn [m [a b]] (if (potential? a b) (-> m (update b conj a) (update a conj b)) m)) {})))

(def dir-tree
  (loop [tree-edges ()
         tree-nodes #{} ;; nodes already added to tree
         node-queue #{"--- scanner 0 ---"}]
    (if-let [n (first node-queue)]
      (let [targets (->> (potential-children n)
                         (remove tree-nodes)
                         (filter (partial try-connect-fn n)))]
        (recur (into tree-edges (for [e targets] [e n]))
               (into tree-nodes (cons n targets))
               (into (disj node-queue n) targets)))
      (reduce (fn [m [k v]] (update m k assoc v (try-connect-fn v k))) {} tree-edges))))

;; returns a function that tells how to map to zero.
(defn floodmap [real-children from]
  (if (= "--- scanner 0 ---" from)
    identity
    (some (fn [[from mapper]]
            (when-let [f (floodmap real-children from)]
              (comp f mapper)))
          (real-children from))))
(def floodmap (memoize floodmap))

(->> (for [[k pts] scanners
           :let [f (floodmap dir-tree k)]
           p pts]
       (f p))
     (set)
     (count)
     (println "First answer:"))

(def scanner->fun
  (into {} (for [s (keys scanners)] [s (floodmap dir-tree s)])))

(let [scanner-positions (for [[s f] scanner->fun] (f [0 0 0]))]
  (->> (for [p1 scanner-positions
             p2 scanner-positions]
         (manhattan p1 p2))
       (apply max)
       (println "Second answer:")))
