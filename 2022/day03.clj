(ns day03)

(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day03.clj"

; (def rules (map #(str/split % #"[ ,;-]") lines))
; (for [line lines :let [[_ a b c] (re-matches #"(\d),(\d),(\d)" line)]] [(Long/parseLong a) b c])

(def prio (-> 
              (zipmap abc (range 1 27))
              (merge (zipmap ABC (range 27 53)))))

(def items (for [line lines
                 :let [f (subs line (/ (count line) 2))
                       s (subs line 0 (/ (count line) 2))]]
  {:second f 
   :first s 
   :line line
   :items (filter (set f) (set s))
   }))

(def sacks (partition 3 items))

(println :second
  (sum
    (for [sack (partition 3 lines)
          comm (reduce set/intersection (map set sack))]
      (prio comm))))
