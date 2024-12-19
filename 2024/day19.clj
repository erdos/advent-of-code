(ns day19)

(let [[a b] (.split (slurp *in*) "\n\n")]
  (def patterns (vec (.split a ", ")))
  (def designs  (vec (.split b "\n"))))

(defn possible-cnt [design]
  (if (empty? design)
    1
    (reduce + (for [pat patterns
                    :when (clojure.string/starts-with? design pat)]
                (possible-cnt (subs design (count pat)))))))

(def possible-cnt (memoize possible-cnt))

(println 'First  (count (remove (comp zero? possible-cnt) designs)))
(println 'Second (transduce (map possible-cnt) + designs))
