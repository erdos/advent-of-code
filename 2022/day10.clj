(ns day10)
(declare data lines)
(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day10.clj"

(def commands
  (for [line lines]
    (if (= line "noop")
      :noop
      (parse-long (subs line 5)))))

(->> commands
     (cons :noop)
     (mapcat (fn [c] (if (= :noop c) [0] [0 c])))
     (reductions + 1)
     (map-indexed (fn [idx v] (* idx v)))
     (drop 20) (take-nth 40) (take 6)
     (sum)
     (println "First solution: "))

(->> commands
     (mapcat (fn [c] (if (= :noop c) [0] [0 c])))
     (reductions + 1)
     (partition 40)
     (map (partial map-indexed (fn [idx x] (if (#{idx (inc idx) (dec idx)} x) \# \.))))
     (run! (partial apply println))
     (do (println "Second solution:")))
