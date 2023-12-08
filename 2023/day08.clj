(ns day08)

(load-file "common.clj")

(def instructions (first lines))

;; map of {location [left-loc right-loc]}
(def tree 
  (into {} (for [line (nnext lines)
                 :let [[_ node left right] (re-matches #"(...) = \((...), (...)\)" line)]]
             [node [left right]])))

(defn solve1 [end? root]
  (->> (cycle instructions)
       (reductions (fn [loc i] 
                     (if (end? loc)
                       (reduced loc)
                       (get-in tree [loc (case i \L 0 \R 1)])))
                   root)
       (count) (dec) (dec)))

(println "First:" (solve1  #{"ZZZ"} "AAA"))

(defn start-node? [n] (str/ends-with? (name n) "A"))
(defn end-node? [n] (str/ends-with? (name n) "Z"))

(->> (keys tree)
     (filter start-node?)
     (map (partial solve1 end-node?))
     (reduce lcm)
     (println "Second:"))
