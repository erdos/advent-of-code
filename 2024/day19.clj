(ns day19)

(let [[a b] (.split (slurp *in*) "\n\n")]
  (def patterns (vec (.split a ", ")))
  (def designs (mapv seq (.split b "\n"))))

(defn starts-with? [prefix v]
  (or (empty? prefix)
      (and (= (first prefix) (first v)) (recur (next prefix) (next v)))))

(defn possible-cnt [design]
  (if (empty? design)
    1
    (reduce + (for [pat patterns :when (starts-with? pat design)]
                (possible-cnt (drop (count pat) design))))))

(def possible-cnt (memoize possible-cnt))

(println 'First  (count (remove (comp zero? possible-cnt) designs)))
(println 'Second (transduce (map possible-cnt) + designs))
