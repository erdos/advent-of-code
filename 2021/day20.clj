#!/usr/bin/env bb

(require '[clojure.string :as s])

(def input (slurp "input20.txt"))
(def lines (s/split-lines input))

(def header (vec (first lines)))
(def maze (mapv vec (nnext lines)))

(defn init-state [maze] {:step 0 :grid maze})

(def blinking? (and (= \# (first header)) (= \. (last header))))

(defn cell-at [{:keys [step grid] :as m} x y]
  (case (get-in grid [y x] (if (and blinking? (odd? step)) \# \.))
    \. 0 \# 1))

(defn code-at [{:keys [cell grid] :as state} x y]
  (let [p (for [i [(dec x) x (inc x)]
                j [(dec y) y (inc y)]]
            [i j])
        bin (for [[x y] p] (cell-at state y x))] ;; flipped!
    (Long/valueOf (apply str bin) 2)))

(defn decoding [code] (assert (integer? code)) (get header code))

(defn resize [{:keys [step grid]}]
  (let [padding (if (and blinking? (odd? step)) \# \.)
        empty-row (vec (repeat (+ 2 (count (first grid))) padding))]
    {:step step
     :grid (vec (concat [empty-row]
                        (for [row grid]
                          (vec (concat [padding] row [padding])))
                        [empty-row]))}))

(defn step [{:keys [step grid] :as old-state}]
  (let [coords (for [y (range (count grid))
                     x (range (count (first grid)))] [x y])]
    {:step (inc step)
     :grid (reduce (fn [new-state [x y]]
                     (let [code (code-at old-state x y)]
                       (assoc-in new-state [x y] (decoding code))))
                   grid
                   coords)}))

(def step (comp step resize))
(defn steps [n state] (nth (iterate step state) n))

(defn count-lit [state] (->> state :grid flatten (filter #{\#}) count))

(->> (init-state maze)
     (steps 2)
     (count-lit)
     (println "First answer:")
     (time))

(->> (init-state maze)
     (steps 50)
     (count-lit)
     (println "Second answer:")
     (time))
