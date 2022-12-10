(require '[clojure.string :as str]
         '[clojure.set :as set]
         '[clojure.test :refer [deftest testing is are]]
    #_     '[loom.graph :as lg])

(defmacro safely [x]
  `(try ~x (catch RuntimeException t# nil)))

(def data (slurp *in*))
(def lines (str/split-lines data))

(safely (def lines (mapv parse-long data)))
(def lines-ints (mapv #(safely (parse-long %)) lines))

(def words (str/split data #"[, ;]"))

(defn sum [xs] (reduce + 0 xs))

(defmacro fnx [body] (list 'fn* '[x] body))

(defn fixpt [f x] (let [fx (f x)] (if (= fx x) x (recur f fx))))

(defn iterations [f elem] (eduction (take-while some?) (iterate f elem)))

(defn assoc-if-val [m k v]
  (if (some? v) (assoc m k v) m))

(defn suffixes [xs] (take-while seq (iterate next xs)))
(defn prefixes [xs] (take-while seq (iterate butlast xs)))

(defn subs-last [^String s ^long n] (.substring s (- (.length s) n)))

(defn filter-first [pred xs] (first (filter pred xs)))

(def abc "abcdefghijklmnopqrstuvwxyz")
(def ABC "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn transpose [mtx]
 (vec (for [i (range (count (first mtx)))] (vec (for [j (range (count mtx))] (-> mtx (nth j) (nth i)))))))

(require '[babashka.deps :as deps])

(deps/add-deps '{:deps {aysylu/loom {:mvn/version "1.0.2"}}})

(require '[loom.graph :as lg :refer [add-nodes add-edges remove-nodes remove-edges subgraph nodes edges successors predecessors graph digraph weighted-graph weighted-digraph]]
         '[loom.alg :as la :refer [dijkstra-path topsort connected-components]])