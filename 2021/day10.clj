#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn ->long [s] (Long/parseLong (str s)))

(def lines (s/split-lines (slurp "input10.txt")))

(defn result-of [x]
  (cond (map? x) nil ; (doto (:score x) #_(println "!"))
        (empty? x) nil
        :else
        (reduce (fn [score c]
                  (+ (* score 5)
                     ((zipmap "([{<" [1 2 3 4]) c))) 0 x)))

(def closing (zipmap "([{<" ")]}>"))
(def scores (zipmap ")]}>" [3 57 1197 25137]))

(defn score [line]
  (result-of
   (reduce
    (fn [stack c]
      (case c
        (\( \[ \{ \<)
        (cons c stack)

        (\) \] \} \>)
        (if (= c (closing (first stack)))
          (next stack)
          (reduced {:score (scores c)}))))
    () line)))

(defn median [xs]
  (let [xs (vec (sort xs))]
    (nth xs (/ (dec (count xs)) 2))))

(println "Second answer:" (median (keep score lines)))

;; not
;; 609402
