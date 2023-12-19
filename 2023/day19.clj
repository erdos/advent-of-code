(ns day19)

(->> *in* slurp (clojure.string/split-lines) (def lines))

(defn parse-e [e]
  (if-let [[bb var op val then] (re-matches #"([a-z]+)([<>])(\d+):([a-zA-Z]+)" e)]
    [(keyword op) var (parse-long val) (case then "A" [:accept] "R" [:reject] [:goto then])]
    (case e "A" [:accept] "R" [:reject] [:goto e])))

(defn- parse-rule [line] 
  (let [[name rest] (.split line "\\{")
        rest (.substring rest 0 (dec (count rest)))
        rest (.split rest ",")]
    [name (mapv parse-e rest)]))

(->> (take-while not-empty lines)
     (map parse-rule)
     (into {})
     (def rules))

(defn- parse-part [line]
  (-> line (.substring 1 (- (count line) 1)) (.split ",")
      (->> (map #(.split % "=")) (map (fn [[a b]] [a (parse-long b)]))
           (into {}))))

(->> (next (drop-while not-empty lines))
     (map parse-part)
     (def parts))

(defn- eval1 [part [x a b c :as cmd]]
    (case x
      :< (when (< (part a) b) c)
      :> (when (> (part a) b) c)
      cmd))

(defn- eval-rule-bodies [part rule-key bodies]
  (let [[a b] (some (partial eval1 part) bodies)]
    (case a
      :accept true
      :reject false
      :goto (recur part b (rules b)))))

(defn- accept? [part] (eval-rule-bodies part "in" (rules "in")))

(defn- rating [part] (apply + (vals part)))

(->> parts
     (filter accept?)
     (map rating)
     (reduce +)
     (println "First"))

(defn- update-rng [[from to] op value]
 (case op :< (when (< from value) [from (min to value)])
          :> (when (< value to)   [(max from value) to])))

(defn- into-ruleset+1 [cube op variable value]
  (when-let [u (update-rng (cube variable) op value)]
    (assoc cube variable u)))

(defn- into-ruleset-1 [cube op variable value]
  (case op
        :> (into-ruleset+1 cube :< variable (inc value))
        :< (into-ruleset+1 cube :> variable (dec value))))

(defn- walk [ruleset bodies]
  (when-let [[[op s v [c d]] & bodies] (seq bodies)]
    (case op
          :accept [ruleset]
          :reject []
          :goto   (recur ruleset (rules s))
          (:< :>) (case c :accept  (cons (into-ruleset+1 ruleset op s v)
                                         (walk (into-ruleset-1 ruleset op s v) bodies))
                          :reject  (recur (into-ruleset-1 ruleset op s v) bodies)
                          :goto    (concat (walk (into-ruleset+1 ruleset op s v) (rules d))
                                           (walk (into-ruleset-1 ruleset op s v) bodies))))))


(defn cube-vol [m] (transduce (map (fn [[a b]] (- b a 1))) * (vals m)))
(def init-cube {"x" [0 4001] "m" [0 4001] "a" [0 4001] "s" [0 4001]})

(->> (walk init-cube (rules "in"))
     (filter some?)
     (map cube-vol)
     (reduce +)
     (println "Second"))
