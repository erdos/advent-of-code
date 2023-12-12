(ns day12)

(load-file "common.clj")

(defn parse-line [line]
  (let [[row nrs] (.split line " ")]
    [(str (.replaceAll row "\\.\\.+" "."))
     (map parse-long (.split nrs ","))]))

(defn valid-suffixes [row number]
  (for [i (range 0 (inc (- (count row) number)))
        :when (every? #{\. \?} (take i row))
        :when (every? #{\# \?} (take number (drop i row)))
        :when (#{\. \?} (nth row (+ i number) \.))]
    (drop (+ i number 1) row)))

(defn acs [row numbers]
  (if-let [[n & nrs] (seq numbers)]
    (reduce + (for [s (valid-suffixes row n)] (acs s nrs)))
    (if (every? #{\. \?} row) 1 0)))

(def acs (memoize acs))

(->> lines
     (map parse-line)
     (map (partial apply acs))
     (reduce +)
     (println "First:")
     (time))
;; First: 8270

(defn *5 [[line rule]]
  (assert (string? line))
  [(clojure.string/join "?" (repeat 5 line))
   (apply concat (repeat 5 rule))])

(->> lines
     (map parse-line)
     (map *5)
     (pmap (partial apply acs))
     (reduce +)
     (println "Second:")
     (time))

;; Second: 204640299929836