(ns day08)

(load-file "common.clj")

(def command (first lines))

;; map of {location [left-loc right-loc]}
(def parsed 
  (into {} (for [line (nnext lines)]
             (let [[a _ [left right]] (read-string (str  "(" line ")"))]
               [a [left right]]))))

(defn solve1 [end? start-loc]
  (->> (cycle command)
       (reductions (fn [loc lr] 
                     (if (end? loc) (reduced loc)
                       (case lr \L (get-in parsed [loc 0])
                                \R (get-in parsed [loc 1]))))
                   start-loc) 
       (count) (dec) (dec)))

(println "First:" (solve1  #{'ZZZ} 'AAA))

(def start-nodes (filter (comp #{\A} last name) (keys parsed)))
(defn end-node? [n] (str/ends-with? (name n) "Z"))

(defn step [lr loc]
  (case lr \L (get-in parsed [loc 0])
           \R (get-in parsed [loc 1])))

(->> start-nodes
     (map (partial solve1 end-node?))
     (reduce lcm)
     (println "Second:"))
