(ns day05)

(load-file "common.clj")

(let [[a b] (clojure.string/split data #"\n\n")]
  (def rules
    (for [line (clojure.string/split a #"\n")]
      (mapv parse-long (clojure.string/split line #"\|"))))
  (def updates
    (for [line (clojure.string/split b #"\n")]
      (mapv parse-long (clojure.string/split line #",")))))

(def rule-following (reduce (fn [m [a b]] (update m a conj b)) {} rules))
(def rule-preceding (reduce (fn [m [a b]] (update m b conj a)) {} rules))

(defn correctly-ordered? [xs]
  (boolean (reduce (fn [seen? x]
                     (if (not-any? seen? (rule-following x))
                       (conj seen? x)
                       (reduced false))) #{} xs)))

(defn middle [xs] (nth xs (/ (count xs) 2)))

(->> updates
     (filter correctly-ordered?)
     (map middle)
     (reduce +)
     (println :First))

(defn fix-order [xs]
  (loop [seen? #{} output [] xs (set xs)]
    (if (seq xs)
      (let [x (some #(when (not-any? xs (rule-preceding %)) %) xs)]
        (recur (conj seen? x) (conj output x) (disj xs x)))
      output)))

(->> updates
     (remove correctly-ordered?)
     (map fix-order)
     (map middle)
     (reduce +)
     (println :Second))