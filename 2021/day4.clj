#!/usr/bin/env bb

(def input (.split (slurp "input4.txt") "\n\n"))

(defn read-nrs [line] (read-string (str "[" line "]")))

(def draw (read-nrs (first input)))
(def boards (mapv read-nrs (next input)))

(defn update-board [board draw]
  (mapv (fn [x] (when-not (= draw x) x)) board))

(defn score-board [board called-nr]
  (* (reduce + (remove nil? board))
     called-nr))

(def already-won (atom #{}))
(def winners (atom []))

(defn print-score [board-idx board called-nr]
  (when-not (contains? @already-won board-idx)
    (swap! winners conj (score-board board called-nr))
    (swap! already-won conj board-idx)))

(defn action-board [board board-idx called-nr]
  (assert (= 25 (count board)))
  (doseq [row (partition 5 board)
          :when (every? nil? row)]
    (print-score board-idx board called-nr))
  (dotimes [i 5]
    (when (every? nil? (for [j (range 5)] (nth board (+ i (* j 5)))))
      (print-score board-idx board called-nr))))

(reduce
 (fn [boards draw]
   (mapv
    (fn [board-idx board]
      (let [updated (update-board board draw)]
        (when-not (= updated board)
          (action-board updated board-idx draw))
        updated))
    (range)
    boards))
 boards
 draw)

(println "First answer:" (first @winners))
(println "Second answer:" (last @winners))
