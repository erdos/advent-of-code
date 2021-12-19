#!/usr/bin/env bb

(require '[clojure.string :as s])

(def lines
  (for [line (vec (s/split-lines (slurp "input12.txt")))
        :let [[_ a b] (re-matches #"(.+)-(.+)" line)]]
    [(if (every? #(Character/isUpperCase %) a)
       (keyword a) (symbol a))
     (if (every? #(Character/isUpperCase %) b)
       (keyword b) (symbol b))]))

(def ways (reduce
           (fn [m [a b]]
             (-> m (update a conj b)
                 (update b conj a)))
           {} lines))

(defn path [current already-visited fav-small]
  (if (= 'end current)
    [[]]
    (for [_ [1]
          [fav-small already-visited]
          (cond (keyword? current)
                [[fav-small already-visited]]

                (and (not= 'start current) (not= 'end current)
                     (symbol? current)
                     (not fav-small))
                [[false (conj already-visited current)]
                 [true already-visited]]

                :else
                [[fav-small (conj already-visited current)]])

          n (remove already-visited (get ways current))
          :when (not (already-visited n))
          tail (path n already-visited fav-small)]
      (cons n tail))))

(->> (count (set (path 'start #{} 'heyy)))
     (println "First answer:"))

(->> (count (set (path 'start #{} nil)))
     (println "Second answer:"))
