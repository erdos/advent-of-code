(ns day18)

(->> *in* slurp (clojure.string/split-lines) (def lines))

(defn parse [line]
  (let [[_ [a] b c] (re-matches #"(.) (\d+) \(#(.{6})\)" line)]
    [a (parse-long b) c]))

(def plan (map parse lines))

(defn solve1 [plan]
  (second (reduce (fn [[h area] [dir len]]
                    (case dir
                      \U [(- h len) area]
                      \D [(+ h len) (+ area len)]
                      \L [h (+ area len (* h len))]
                      \R [h (- area (* h len))]))
                  [0 1] plan)))

(->> plan solve1 (println "First"))

(defn map-plan [[_ _ dir]]
  [((zipmap "0123" "RDLU") (last dir))
   (read-string (apply str "0x" (butlast dir)))])

(->> plan (map map-plan) solve1 (println "Second"))