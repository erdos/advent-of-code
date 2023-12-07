(ns day07)

(load-file "common.clj")

(def labels "AKQJT98765432") ;; A is highest

(def card->score (zipmap labels (map - (range))))

(defn parse [line] 
  (let [[a b] (.split (str line) " ")]
    [a (parse-long b)]))

(def input (map parse lines))
(def hand->bid (into {} input))

(defn type-score [hand]
  ({[5] 7, [1 4] 6, [2 3] 5, [1 1 3] 4, [1 2 2] 3, [1 1 1 2] 2, [1 1 1 1 1] 1}
   (sort (vals (frequencies hand)))))

(defn hand-score [hand]
  (vec (cons (+ (type-score hand)) (map card->score hand))))

(->> (zipmap (sort-by hand-score (map first input)) (next (range)))
     (reduce-kv (fn [m hand rank] (+ m (* rank (hand->bid hand))) ) 0)
     (println "First"))

(def card->score-joker (zipmap "AKQT98765432J" (map - (range)))) ;; J comes last now

(defn most-common-letter [hand]
  (if (= "JJJJJ" hand)
    \J (apply max-key (frequencies hand) (remove #{\J} hand))))

(defn type-score-with-joker [hand]
  (type-score (.replaceAll hand "J" (str (most-common-letter hand)))))

(defn hand-score-joker [hand]
  `[~(type-score-with-joker hand) ~@(map card->score-joker hand)])

(->> (zipmap (sort-by hand-score-joker (map first input)) (next (range)))
     (reduce-kv (fn [m hand rank] (+ m (* rank (hand->bid hand)))) 0)
     (println "Second"))
