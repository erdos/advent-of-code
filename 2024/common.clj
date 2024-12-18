(require '[clojure.string :as str]
         '[clojure.set :as set]
         '[clojure.test :refer [deftest testing is are]])

(defmacro safely [x]
  `(try ~x (catch RuntimeException t# nil)))

(def data (slurp *in*))
(def lines (str/split-lines data))
(def grid (mapv vec lines))

(safely (def lines (mapv parse-long data)))
(safely (def lines-ints (mapv parse-long lines)))

(def words (str/split data #"[, ;]"))

(defn read-numbers [s] (read-string (str "[" s "]")))

(defn sum [xs] (reduce + 0 xs))

(defmacro fnx [body] (list 'fn* '[x] body))

(defn fixpt [f x] (let [fx (f x)] (if (= fx x) x (recur f fx))))

(defn iterations [f elem] (eduction (take-while some?) (iterate f elem)))

(defn assoc-some [m k v] (if (some? v) (assoc m k v) m))

(defn suffixes [xs] (take-while seq (iterate next xs)))
(defn prefixes [xs] (take-while seq (iterate butlast xs)))

(defn subs-last [^String s ^long n] (.substring s (- (.length s) n)))

(defn filter-first [pred xs] (first (filter pred xs)))

(def abc "abcdefghijklmnopqrstuvwxyz")
(def ABC "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(def digits (set "0123456789"))

(defn transpose [mtx]
  (vec (for [i (range (count (first mtx)))] (vec (for [j (range (count mtx))] (-> mtx (nth j) (nth i)))))))

(defn consuming [f sequence]
  (when (seq sequence)
    (when-let [[v tail] (f sequence)]
      (cons v (lazy-seq (consuming f tail))))))

;(require '[babashka.deps :as deps])

;(deps/add-deps '{:deps {aysylu/loom {:mvn/version "1.0.2"}}})

#_(require '[loom.graph :as lg :refer [add-nodes add-edges remove-nodes remove-edges subgraph nodes edges successors predecessors graph digraph weighted-graph weighted-digraph]]
           '[loom.alg :as la :refer [dijkstra-path topsort connected-components]])

(defn gcd [a b] (if (zero? b) a (recur b, (mod a b))))
(defn lcm [a b] (/ (* a b) (gcd a b)))
(defn lcmv [& v] (reduce lcm v))

(def mapv-indexed (comp vec map-indexed))

(defn splitsearch [predicate range]
  (assert (vector? range))
  (loop [low  0, high (count range)]
    (if (< low high)
      (let [mid (quot (+ low high) 2)]
        (if (predicate mid)
          (recur low mid)
          (recur (inc mid) high)))
      (when (predicate low) low))))