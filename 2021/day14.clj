#!/usr/bin/env bb

(require '[clojure.string :as s])

(def lines (vec (s/split-lines (slurp "input14.txt"))))

(def header (first lines))

(def data
  (reduce (fn [m [[a b] target]] (assoc-in m [a b] target))
          {}
          (for [line (nnext lines)
                :let [[_ a b] (re-matches #"(..) -> (.)" line)]]
            [(vec a) (first b)])))

(defn insertion [header]
  (cons (first header)
        (mapcat (fn [a b]
                  (if-let [c (get-in data [a b])]
                    [c b]
                    [b]))
                header
                (next header))))

(defn bigrams [header]
  [(frequencies header)
   (frequencies (map vector header (next header)))])

(defn freqiter [freqs]
  (reduce (fn [[letter-freqs m] [[a b] fs]]
            (if-let [c (get-in data [a b])]
              [(-> letter-freqs
                   (update c (fnil + 0) fs))
               (-> m
                   (update [a c] (fnil + 0) fs)
                   (update [c b] (fnil + 0) fs))]
              [letter-freqs
               (update m [a b] (fnil + 0) fs)]))
          [(first freqs) {}]
          (second freqs)))

(defn solve [steps]
  (let [cf (iterate freqiter (bigrams  header))
        [lns cf] (nth cf steps)]
    (- (apply max (vals lns)) (apply min (vals lns)))) )

(println "First answer:" (solve 10))
(println "First answer:" (solve 40))
