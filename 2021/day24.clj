#!/usr/bin/env clojure

(defonce numbers (map read-string (re-seq #"-?\d+" (slurp "input24.txt"))))

(defonce config
  (for [[_ _ zdiv xadd _ _ _ _ _ yadd] (partition 10 numbers)]
    [zdiv xadd yadd]))

(defn- evaluate [z w [zdiv xadd yadd]]
  (let [x (+ (rem z 26) xadd)
        z (quot z zdiv)
        x (if (= x w) 0 1)]
    (+ (* z (+ (* 25 x) 1))
       (* x (+ w yadd)))))

(defn solver [z config depth digits]
  (if-let [[c & tail] (not-empty config)]
    (when-not (and (zero? depth) (not (zero? z)))
      (let [next-depth (if (= (first c) 1) (inc depth) (dec depth))]
        (for [r digits
              s (solver (evaluate z r c) tail next-depth digits)]
          (cons r s))))
    (when (= z 0)
      [[]])))
(def solver (memoize solver))

(->> (range 9 0 -1)
     (solver 0 config 0)
     (first)
     (apply str)
     (time)
     (println "First answer:" ))

(->> (range 1 10)
     (solver 0 config 0)
     (first)
     (apply str)
     (time)
     (println "Second answer:" ))
