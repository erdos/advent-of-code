(ns day11)
(declare data lines)
(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day10.clj"

;(def commands
;  (for [line (next (drop-while not-empty lines))
;        :let [[_ cnt from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]]
;    [(parse-long cnt) (dec (parse-long from)) (dec (parse-long to))]))

(def inspections (atom {}))

(declare common)

(defn do-monkey [monkey-id monkeys]
  (assert (string? monkey-id) (pr-str monkey-id))
  (reduce (fn [monkeys item]
            (swap! inspections update monkey-id (fnil inc 0))
            ;(println " item" item)
            (let [monkey (monkeys monkey-id)
                  op     (:op monkey)
                  item   (op item)
                  item (bigint (rem (bigint item) common)) ;; NOT
                  div-test (:div-test monkey)
                  dived  (zero? (rem item div-test))]
            ;(println "  dived?" dived)
              (if dived
                (update monkeys (:then monkey) update :items conj item)
                (update monkeys (:else monkey) update :items conj item))))
          (update monkeys monkey-id assoc :items [])
          (get-in monkeys [monkey-id :items])))

(defn do-monkeys [monkeys]
  (reduce (fn [monkeys monkey]
            (do-monkey (:id monkey) monkeys))
          monkeys (vals monkeys)))

(def monkey-map
  (->>
   (for [[id items op test then else] (partition-all 7 lines)]
     {:id (apply str (butlast (subs id 7)))
      :items (map (comp bigint parse-long) (str/split (subs items 18) #", "))
      :op (let [[_ op right]
                (re-matches #"new = old (.) (.+)" (subs op 13))]
            (fn [old]
              (({"+" + "*" *} op)
               old (or (parse-long right) old))))
      :div-test (parse-long (subs test 21))
      :then (do (subs then 29))
      :else (do (subs else 30))})
   (reduce (fn [t m] (assoc t (:id m) m)) (sorted-map))))

(def common (apply * (map :div-test (vals monkey-map))))

(def rounds 10000)

(->>
 monkey-map
 ((fn [monkeys] (nth (iterate do-monkeys monkeys) rounds)))
 doall
 time)

(->> @inspections
     vals
     sort
     reverse
     (take 2) (apply *)
     println)
