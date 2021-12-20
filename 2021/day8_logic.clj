#!/usr/bin/env clojure

(require '[clojure.core.logic :as logic])

(def lines
  (for [line (clojure.string/split-lines (slurp "input8.txt"))
        :let [[data test] (clojure.string/split line #" \| ")]]
    [(clojure.string/split data #" ") (clojure.string/split test #" ")]))

(defn- segments [xs digit & segments]
  (logic/all (logic/member1o digit xs)
             (logic/permuteo digit (vec segments))))

(defn get-model [xs]
  (first (logic/run 1 [dig0 dig1 dig2 dig3 dig4 dig5 dig6 dig7 dig8 dig9]
                    (logic/fresh [aaaa bbbb cccc dddd eeee ffff gggg]
                                 (segments xs dig1 cccc ffff)
                                 (segments xs dig7 aaaa cccc ffff)
                                 (segments xs dig4 bbbb cccc dddd ffff)
                                 (segments xs dig3 aaaa cccc dddd ffff gggg)
                                 (segments xs dig5 aaaa bbbb dddd ffff gggg)
                                 (segments xs dig2 aaaa cccc dddd eeee gggg)
                                 (segments xs dig6 aaaa bbbb dddd eeee ffff gggg)
                                 (segments xs dig9 aaaa bbbb cccc dddd ffff gggg)
                                 (segments xs dig0 aaaa bbbb cccc eeee ffff gggg)
                                 (segments xs dig8 aaaa bbbb cccc dddd eeee ffff gggg)))))

(->> (for [[model b] (pmap (fn [[a b]] [(get-model (map vec a)) (map set b)]) lines)
           :let [set->digit (reduce-kv (fn [m i v] (assoc m (set v) i)) {} model)]]
       (Long/parseLong (apply str (map set->digit b))))
     (reduce +)
     (time)
     (println "Second answer:"))

(shutdown-agents)
