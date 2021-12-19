#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn ->long [s] (Long/parseLong s))

(def all-input (slurp "input13.txt"))
(def all-lines (s/split-lines all-input))

(def lines
  (for [line (take-while seq all-lines)
        :let [[_ a b] (re-matches #"(\d+),(\d+)" line)]]
    [(->long a) (->long b)]))

(defn state-of [lines] (set lines))

(defn fold-x [state t]
  (set (for [[x y] state :when (not= x t)]
         (if (> x t) [(- (+ t t) x) y] [x y]))))

(defn fold-y [state t]
  (set (for [[x y] state :when (not= y t)]
         (if (> y t) [x (- (+ t t) y)] [x y]))))

(def folds
  (for [[_ axis len] (re-seq #"fold along (.)=(\d+)" all-input )]
    [(keyword axis) (->long len)]))

(->> (count  (fold-x (state-of lines) 655))
     (println "First answer:"))

(defn draw [pts]
  (let [x (apply max (map first pts))
        y (apply max (map second pts))
        s (vec (repeat (inc y) (vec (repeat (inc x) " "))))]
    (reduce (fn [s [x y]] (assoc-in s [y x] "█")) s pts)))

(println "Second answer:")
(-> (reduce (fn [m [x l]]
              (case x :x (fold-x m l)
                    :y (fold-y m l))) (state-of lines) folds)
    (draw)
    ((partial run! (comp println (partial apply str)))))
