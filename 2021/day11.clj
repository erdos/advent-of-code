#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn ->long [s] (Long/parseLong s))

(def lines (mapv (fn [line] (mapv (comp ->long str) line) ) (s/split-lines (slurp "input11.txt"))))

(defn inc-all [lines]
  (mapv (fn [line] (mapv inc line)) lines))

(defn update-in? [x path f]
  (assert (every? int? path))
  (if (get-in x path)
    (update-in x path f)
    x))

(def total-flashes (atom 0))
(def flashed (atom #{}))

(defn flash-at [lines i j]
  (swap! flashed conj [i j])
  (swap! total-flashes inc)
  (-> lines
      (update-in? [i (inc j)] inc)
      (update-in? [i (dec j)] inc)

      (update-in? [(dec i) (dec j)] inc)
      (update-in? [(dec i) (inc j)] inc)
      (update-in? [(inc i) (dec j)] inc)
      (update-in? [(inc i) (inc j)] inc)

      (update-in? [(inc i) j] inc)
      (update-in? [(dec i) j] inc)))

(defn fixpt [f x] (let [fx (f x)] (if (= x fx) x (recur f fx))))

(defn converge [lines]
  (let [positions (for [i (range (count lines))
                        j (range (count (first lines)))
                        :when (> (get-in lines [i j]) 9)
                        :when (not (contains? @flashed [i j]))]
                    [i j])]
    (reduce (fn [lines [i j]] (flash-at lines i j)) lines positions)))

(defn reset-flashed [lines]
  (let [lines (reduce (fn [lines [i j]]
                        (assoc-in lines [i j] 0))
                      lines
                      @flashed)]
    (reset! flashed #{})
    lines))

(defn step [lines]
  (reset! flashed #{})
  (->> lines
       (inc-all)
       (fixpt converge)
       (reset-flashed)))

(nth (iterate step lines) 100)
(println "First answer:" @total-flashes)

(->> (count (take-while (partial some (partial some pos?))
                        (iterate step lines)))
     (println "Second answer: "))
