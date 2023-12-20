(ns day20)

(defn parse [line]
  (let [[_ type name targets] (re-matches #"([&%]?)([a-z]+) -> (.*)" line)]
    [name [({"&" :conj "%" :ff "" :src} type)
           (clojure.string/split targets #", ")]]))

(->> *in* (slurp) (clojure.string/split-lines)
     (map parse)
     (into {"button" [:src ["broadcaster"]]})
     (def config))

(->> (for [[src [_ targets]] config, t targets] [t src])
     (reduce (fn [m [t src]] (update m t conj src)) {})
     (def sources))

(def initial-state
  (into {} (for [[src [t _]] config]
             [src (case t :src  nil
                          :ff   false
                          :conj (zipmap (sources src) (repeat :low)))])))

(def total (atom {:low 0 :high 0}))
(def counts (atom {}))

(defn sending [sn type]
  (let [ts (second (config sn))]
    (swap! total update type + (count ts))
    (when (= :low type)
      (swap! counts (fn [counts] (reduce (fn [counts t] (update counts t (fnil inc 0))) counts ts))))
    (for [t ts] [t type sn])))

(defn simulate [state]
  (loop [state   state
         signals (sending "button" :low)]
  (if-let [[[sn st sig-src] & ss] (seq signals)]
    (let [[type targets] (config sn)]
      ;; (println '-- sn '--- st '---> targets)
      (case type
        nil  (recur state ss)
        :src (recur state (into ss (sending sn st)))
        :ff  (case st
               :high (recur state ss)
               :low  (->> (if (state sn) :low :high)
                          (sending sn)
                          (concat ss)
                          (recur (update state sn not))))
        :conj (let [mem (assoc (state sn) sig-src st)]
                (->> (if (every? #{:high} (vals mem)) :low :high)
                     (sending sn)
                     (concat ss)
                     (recur (assoc state sn mem))))))
    state)))

(-> initial-state
     (->> (iterate simulate))
     (nth 1000))

(->> @total vals (reduce *) (println "First"))

(defn gcd [a b] (if (zero? b) a (recur b, (mod a b))))
(defn lcm [a b] (/ (* a b) (gcd a b)))
(defn lcmv [& v] (reduce lcm v))

(->> (iterate simulate initial-state)
     (reduce (fn [[tick st tn hh dt] _]
               (if (and st tn hh dt)
                 (reduced (lcmv st tn hh dt))
                 [(inc tick)
                  (or st (if (= 1 (@counts "st")) tick))
                  (or tn (if (= 1 (@counts "tn")) tick))
                  (or hh (if (= 1 (@counts "hh")) tick))
                  (or dt (if (= 1 (@counts "dt")) tick))]))
             [0])
     (println "Second"))