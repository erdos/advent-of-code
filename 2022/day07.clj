(ns day07)

(declare data lines)

(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day03.clj"

;(def config (butlast (take-while not-empty lines)))
;(def config (for [c config] (vec (take-nth 4 (next c) ))))

;(def commands
;  (for [line (next (drop-while not-empty lines))
;        :let [[_ cnt from to] (re-matches #"move (\d+) from (\d+) to (\d+)" line)]]
;    [(parse-long cnt) (dec (parse-long from)) (dec (parse-long to))]))

(def chunks
  ((fn f [lines]
     (when-let [[head & tail] (seq lines)]
       (let [[a b] (split-with (fn [line] (not (.startsWith line "$ "))) tail)]
         (cons (cond (= head "$ cd ..") :up
                     (.startsWith head "$ cd ") {:cd (subs head 5)}
                     (= head "$ ls")
                     {:ls
                      (into {}
                            (for [line a
                                  :let [[e f] (read-string (str "(" line ")"))]]
                              (if (= e 'dir)
                                [(str f) {}]
                                [(str f) e])))}
                     :else (assert false (str head)))
               (lazy-seq (f b))))))
   lines))
chunks

(defn totsi [tree]
  (if (map? tree)
    (let [tree (zipmap (keys tree) (map totsi (vals tree)))]
      (assoc tree :size (sum (for [[k v] tree :when (string? k)]
                               (if (number? v) v (:size v))))))
    tree))

(->>
 (reduce
  (fn [[path map] cmd]
    (cond
      (= :up cmd) [(next path) map]
      (:cd cmd)   [(if (= "/" (:cd cmd)) '("/") (cons (:cd cmd) path)) map]
      (:ls cmd)
      [path
       (reduce (fn [m [name attr]]
                 (if (number? attr)
                   (update-in m (reverse path) (fnil assoc {:path path}) name attr)
                   m))
               map
               (:ls cmd))]))
  '[("/") {}]
  chunks)
 (second)
 totsi
 (def supertree))

(def currentfreespace (- 70000000 (:size supertree)))

(->>
 supertree
 (tree-seq map? vals) (filter map?)
 (sort-by :size)
 (some (fn [m]
         (when (>= (+ currentfreespace (:size m)) 30000000)
           (:size m))))
 println)
