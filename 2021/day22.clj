#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn ->long [x] (Long/valueOf x))

(def items (for [[c & xs]
                 (partition 7 (re-seq #"on|off|-\d+|\d+" (slurp "input22test.txt")))]
             [(keyword c) (partition 2 (map ->long xs))]))

(defn points [[[a b] [c d] [e f]]]
  (for [i (range (max a -50) (inc (min 50 b)))
        j (range (max c -50) (inc (min 50 d)))
        k (range (max e -50) (inc (min 50 f)))]
    [i j k]))

(->> items
     (reduce (fn [s [cmd region]]
               (println :state-volume (count s))
               (case cmd
                 :on (apply conj s (points region))
                 :off (apply disj s (points region))))
             #{})
     (count)
     (println "First answer:"))

;; SECOND PART

(defn realpoints [& triples]
  (filter (fn [[a b cmd]] (<= a b)) triples))

(defn overlap-segment [[a b] [c d]]
  {:pre [(<= a b) (<= c d)]
   :post (every? (for [[x y _] %] (<= x y)))}
  (assert (<= a b) (<= c d))
  (cond

    ;; no intersection
    (< b c) (list [a b :first] #_[b c :none] [c d :second])
    (< d a) (list [c d :second] #_[d a :none] [a b :first])

    ;; little intersection
    (<= a c b d) (realpoints [a (dec c) :first] [c b :both] [(inc b) d :second])
    (<= c a d b) (realpoints [c (dec a) :second] [a d :both] [(inc d) b :second])

    ;; one contains another
    (<= a c d b) (realpoints [a (dec c) :first] [c d :both] [(inc d) b :first])
    (<= c a b d) (realpoints [c (dec a) :second] [a b :both] [(inc b) d :second])

    :else (assert false (str "Unexpected coords " (pr-str [[a b] [c d]])))))

(defn same-code? [a b c] (->> [a b c] (remove #{:both}) ((fn [x] (or (empty? x) (apply = x))))))
(def same-code (memoize same-code?))

(defn union-cubes [xs ys]
  (let [[a b c] (map overlap-segment xs ys)]
    (for [[x1 x2 xcode] a
          [y1 y2 ycode] b
          [z1 z2 zcode] c
          :when (same-code? xcode ycode zcode)]
      [[x1 x2] [y1 y2] [z1 z2]])))

(defn subtract-cubes [xs ys]
  (let [[a b c] (map overlap-segment xs ys)]
    (for [[x1 x2 xcode] a
          [y1 y2 ycode] b
          [z1 z2 zcode] c
          :when (= :first xcode ycode zcode)]
      [[x1 x2] [y1 y2] [z1 z2]])))

(defn cube-volume [sides] (reduce * (map (fn [[a b]] (inc (- b a))) sides)))

(->> items
     (reduce (fn [s [cmd region]]
               (assert (= (count s) (count (set s))))
               (println :state-size (count s))
               (println " ":state-volume (->> s (mapcat points) count))
               ; (println :state s)
               (case cmd
                 :on (if (empty? s)
                       (list region)
                       (mapcat (partial union-cubes region) s))
                 :off  (mapcat (fn [oldcube] (subtract-cubes oldcube region)) s)))
             ())
     (mapcat points) (set) (count) (println :testing-second-answer)
     ;; (map cube-volume) (reduce + 1N) (println)

     )
