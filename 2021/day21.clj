#!/usr/bin/env bb

(def player1 6)
(def player2 3)

(def rolls (atom 0))
(def dicestate (atom -1))

(defn roll! []
  (swap! rolls inc)
  (swap! dicestate inc)
  (swap! dicestate mod 100)
  (inc @dicestate))

(defn step [current-pos rolled]
  (inc (mod (+ (dec current-pos) rolled) 10)))

(defn roll3! [] (+ (roll!) (roll!) (roll!)))

(loop [[player1 player2] [player1 player2]
       [score1 score2] [0 0]]
  (let [rolled (roll3!)
        new-loc (step player1 rolled)
        new-score (+ score1 new-loc)]
    (if (>= new-score 1000)
      (println "First answer:" (* score2 @rolls))
      (recur [player2 new-loc]
             [score2 new-score]))))

(def bin3 (frequencies (for [i [1 2 3] j [1 2 3] k [1 2 3]] (+ i j k))))

(defn solve [[pos1 score1] [pos2 score2]]
  (if (>= score2 21)
    [(long 0) (long 1)]
    (let [xs (for [[dice freq] bin3
                   :let [next-pos (step pos1 dice)
                         next-score (+ score1 next-pos)
                         [sol1 sol2] (solve [pos2 score2] [next-pos next-score])]]
               [(* freq sol2)
                (* freq sol1)])]
      (reduce (fn [[a b] [c d]] [(+ a c) (+ b d)]) xs))))

(def solve (memoize solve))

(->> (solve [6 0] [3 0])
     (apply max)
     (println "Second answer:"))
