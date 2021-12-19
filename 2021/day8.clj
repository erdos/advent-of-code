(require '[clojure.string :as s])

(def lines
  (for [line (vec (s/split-lines (slurp "input8.txt")))
        :let [[a b] (s/split line #" \| ")]
        :let [inputs (s/split a #" ")
              outputs (s/split b #" ")]]
    [(set (map set inputs)) (vec outputs)]))

(defn- only [x] (if (= 1 (count x)) (first x) (assert false (str (count x)))))

(defn model [segments]
  (assert (set? segments))
  (let [digit1 (set (only (filter (comp #{2} count) segments)))
        digit7 (set (only (filter (comp #{3} count) segments)))
        digit8 (set (only (filter (comp #{7} count) segments))) ;; all are on
        digit6 (only (for [s segments
                           :when (= 6 (count s))
                           :when (= 1 (count (filter digit1 s)))]
                       (set s)))
        digit5 (only (for [s segments
                           :when (= 5 (count s))
                           :when (every? digit6 s)]
                       (set s)))
        aaaa (only (remove digit1 digit7))
        cccc (only (remove digit6 digit8))
        eeee (only (remove digit5 digit6))
        digit0 (only (for [s segments
                           :when (= 6 (count s))
                           :when (= 2 (count (filter digit1 s)))
                           :when (some #{eeee} s)]
                       (set s)))
        dddd (only (remove digit0 digit8))
        ffff (only (filter digit6 digit1))
        digit4 (only (for [s segments :when (= 4 (count s))] (set s)))
        digit2 (only (for [s segments :when (= 5 (count s))
                           :when (every? s [aaaa cccc dddd eeee])]
                       (set s)))
        gggg (only (remove #{aaaa cccc dddd eeee} digit2))
        bbbb (only (remove #{aaaa cccc dddd eeee ffff gggg} "abcdefg"))]
    [#{aaaa bbbb cccc eeee ffff gggg}
     #{cccc ffff}
     #{aaaa cccc dddd eeee gggg}
     #{aaaa cccc dddd ffff gggg}
     #{bbbb cccc dddd ffff}
     #{aaaa bbbb dddd ffff gggg}
     #{aaaa bbbb dddd eeee ffff gggg}
     #{aaaa cccc ffff}
     #{aaaa bbbb cccc dddd eeee ffff gggg}
     #{aaaa bbbb cccc dddd ffff gggg}]))

(defn render [m word]
  (only (for [i (range 10)
              :when (= (set word) (m i))]
          i)))

(->> (count (for [[_ outputs] lines
                  o outputs
                  :when (#{7 2 4 3} (count o))]
              o))
     (println "First answer:"))

(->> (reduce +
             (for [[inputs outputs] lines
                   :let [model (model inputs)
                         numbers (map (partial render model) outputs)
                         nr (Long/parseLong (apply str numbers))]]
               nr))
     (println "Second answer:"))
