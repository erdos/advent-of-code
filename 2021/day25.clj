#!/usr/bin/env bb

(set! *warn-on-reflection* true)

(require '[clojure.string :as s])

(defn ->long [x] (Long/valueOf ^String x))
(defn third [x] (nth x 2))

(def data (slurp "input25.txt"))

(def lines (mapv vec (s/split-lines data)))
(def width (count (first lines)))
(def height (count lines))

(defn step-right-1 [line]
  (reduce (fn [new-line i]
            (let [next-pos (mod (inc i) width)]
              (if (= \. (line next-pos))
                (assoc new-line i \. next-pos \>)
                new-line)))
          line
          (for [i (range width)
                :when (= \> (line i))]
            i)))

(defn step-right [lines] (mapv step-right-1 lines))

(defn step-down [lines]
  (reduce
   (fn [new-lines [i j]]
     (let [i' (mod (inc i) height)]
       (if (= \. (get-in lines [i' j]))
         (-> new-lines
             (assoc-in [i j] \.)
             (assoc-in [i' j] \v))
         new-lines)))
   lines
   (for [i (range height)
         j (range width)
         :when (= \v (get-in lines [i j]))]
     [i j])))

(def step (comp step-down step-right))

(->> (iterate step lines)
     (partition 2 1)
     (take-while (partial apply not=))
     (count)
     (inc)
     (println "Answer:")
     (time))
