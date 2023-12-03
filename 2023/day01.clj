(ns day01)

(load-file "common.clj")

(def digits (set "0123456789"))

(->>
 (for [line lines]
   (parse-long (str (first (filter digits line))
                    (last (filter digits line)))))
 (sum)
 (println "First"))

(def words {"zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9 "ten" 10})

(defn fix-start [s]
  (if-let [idx (seq (for [[k v] words :let [idx (str/index-of s k)] :when idx] [idx k v]))]
    (let [[_ name value] (apply min-key first idx)]
      (recur (.replace s name (str value))))
    s))

(defn fix-end [s]
  (if-let [idx (seq (for [[k v] words :let [idx (str/last-index-of s k)] :when idx] [idx k v]))]
    (let [[_ name value] (apply max-key first idx)]
      (recur (.replace s name (str value))))
    s))

(->>
 (for [line lines]
   (parse-long (str (first (filter digits (fix-start line)))
                    (last (filter digits (fix-end line))))))
 (sum)
 (println "Second"))
