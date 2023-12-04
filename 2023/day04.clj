(ns day04)

(load-file "common.clj")

(defn parse-line [line]
  (let [;x (re-matches #"Card (\d+)" line )
        [a b] (str/split line #": ")
        [_ n] (str/split a #" +")
        [aa bb] (str/split b #" \| ")]
    {:n (parse-long n)
     :own (read-string (str "[" aa "]"))
     :winning (read-string (str "[" bb "]"))}))

(defn score [x]
;  (Math/pow 2 (count ))
  (some->> (filter (set (:own x)) (:winning x)) seq
           count
           dec
           (Math/pow 2) long))

(->> lines
     (mapv parse-line)
     (keep score)
     sum
     (println "First:"))

(def cards (mapv parse-line lines))

(def initial-card-counts (zipmap (map :n cards) (repeat 1M)))

(defn score-round [card-counts {:keys [n own winning]}]
  (let [cur-card-cnt (card-counts n 0)
        win-cnt      (count (filter (set own) winning))
        extras       (range (+ 1 n) (min (+ n win-cnt 1) (+ 1 (count cards))))]
    (reduce (fn [cnt extra] (update cnt (long extra) + cur-card-cnt)) card-counts extras)))

(->> cards
     (reduce score-round initial-card-counts)
     vals sum long
     (println "Second:"))
