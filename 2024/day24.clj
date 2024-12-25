(ns day24)
; (load-file "common.clj")

(require '[loom.graph :as lg :refer [add-nodes add-edges remove-nodes remove-edges subgraph nodes edges successors predecessors graph digraph weighted-graph weighted-digraph]]
         '[loom.alg :as la :refer [dijkstra-path topsort connected-components]])

(def data (slurp "data.txt"))
(declare data)

(def z-keys (for [i (range 46)] (format "z%02d" i)))
(def x-keys (for [i (range 45)] (format "x%02d" i)))
(def y-keys (for [i (range 45)] (format "y%02d" i)))

(let [[as bs] (.split data "\n\n")]
  (def initial (into {} (for [a (.split as "\n")] (vec (.split a ": ")))))
  (def exprs0
    (for [b (.split bs "\n")
          :let [[_ a op b c] (re-matches #"([^ ]+) (OR|XOR|AND) ([^ ]+) -> ([^ ]+)" b)
                [a b] (sort [a b])]]
      [op a b c])))

(def fixes {"z16" "hmk" "hmk" "z16"
            "z20" "fhp" "fhp" "z20"
            "z33" "fcd" "fcd" "z33"
            "rvf" "tpc" "tpc" "rvf"}
;;;  fcd,fhp,hmk,rvf,tpc,z16,z20,z33
  )

(def exprs (for [[op a b target] exprs0]
             [op a b (fixes target target)]))


(println "digraph {")


(println "subgraph xs {")
(println "bgcolor=\"red\"")
(doseq [x x-keys] (println x))
(println "}")
(doseq [[a b] (partition 2 1 x-keys)]
  (println a '-> b "[color=yellow, weight=2];"))

(doseq [[a b] (partition 2 1 z-keys)]
  (println a '-> b "[color=cyan, weight=3];"))
(doseq [[a b] (partition 2 1 y-keys)]
  (println a '-> b "[color=magenta, weight=2];"))


(doseq [[z y x] (map vector z-keys y-keys x-keys)]
  (println "subgraph {")
  (printf "{rank=same; %s;%s;%s;}\n" z y x)
  (println z '-> y "[color=white];")
  (println y '-> x "[color=white];")
  (println z '-> x "[color=white];")
  (println "}"))

(doseq [[op a b target] exprs
        :let [inter (str op '_ a '_ b)]]
  (println inter "[label= " op ", shape=plaintext];")
  (println a '-> inter)
  (println b '-> inter)
  (println inter '-> target))
(println "}")

(def g (apply lg/digraph (for [[_ a b c] exprs a [a b]] [a c])))

(def expr<-calc (into {} (for [[op a b result] exprs] [result [op a b]])))
(assert (= (count expr<-calc) (count exprs)))

(defn eval1 [values [op a b]]
  (case op
    "AND" ({["1" "1"] "1"} [(values a) (values b)] "0")
    "OR"  ({["0" "0"] "0"} [(values a) (values b)] "1")
    "XOR" (if (not= (values a) (values b)) "1" "0")))

(defn eval-binary [initial]
  (->> (topsort g)
       (reduce (fn [values v]
                 (assert (map? values) (str "Not map" values))
                      ;(println :v v)
                 (if-let [calc (expr<-calc v)]
                   (assoc values v (eval1 values calc))
                   values))
               initial)
       (sort)
       (filter (fn [[k]] (.startsWith k "z")))
       (map second) reverse
       (apply str)))

#_
(->> (eval-binary (merge {}
                         (zipmap x-keys (repeat "1"))
                         (zipmap y-keys (repeat "1"))))
;     (#(Long/parseLong % 2))
     (println :table))



(System/exit -3)


;; otlet: felepitek egty binaris halot en magam is.
;; aztan megnezem mennyi drot nem stimmel.

;; (println (topsort g))
(->> (topsort g)
     (reduce (fn [values v]
               (assert (map? values) (str "Not map" values))
               ;(println :v v)
               (if-let [[op a b] (expr<-calc v)]
                 (assoc values v (with-meta (list op (values a) (values b))
                                   {:wire v}))
                 values))
             (zipmap (keys initial) (keys initial)))
     (sort)
     (filter (fn [[k]] (.startsWith k "z")))
     ;(map second)
     ;first
     (into (sorted-map))
     (def wiring))

;; sorted
(def z-wires (keys wiring))

;; mindegyik legyen xor-os
; (assert (every? (comp #{"XOR"} first) (butlast z-wires)))

(doseq [[k [op a b]] (butlast wiring)] ;; last is xor, others are OR
  (when (not= "XOR" op)
    (println "rule 1) Mismatch for" k)))
;; these are defo wrong: z16 z20 z33

;; OR szulei mindig AND-ok!!!
(doseq [[op a b _] exprs
        :when (= "OR" op)
        :when (coll? (wiring a))
        :when (coll? (wiring b))]
  (when (not= "AND" (first (wiring a))) (println "rule 2) mismatch for " a))
  (when (not= "AND" (first (wiring b))) (println "rule 2) mismatch for " b)))

;; XOR-ral mindig van parhuzamosan kotve egy AND es vice versa.

(def xor-parents
  (into {} (for [[op a b target] exprs :when (= "XOR" op)]
             [target #{a b}])))

(def and-parents
  (into {} (for [[op a b target] exprs :when (= "AND" op)] [target #{a b}])))

(assert (= (set (vals xor-parents)) (set (vals and-parents))))

(defn depth [tree]
  (if (string? tree) 0
      (inc (max (depth (nth tree 1))
                (depth (nth tree 2))))))

(println :! (map depth (vals wiring)))

;; adding up 45-bit numbers. x00-x44 + y00-x44 -> (z00-z45)

;; z45 must be carry.

;; return a pair of [s Cout]
(defn xor [a b] (cond (= a b) "0" (= "0" a) b (= "0" b) a :else ["XOR" a b]))
(defn wire-and [a b]
  (cond (= a b) a
        (= "0" a) "0"
        (= "0" b) "0" (= "1" a) b (= "1" b) a :else ["AND" a b]))
(defn wire-or [a b] (cond (= a b) a
                          (= "0" a) b (= "0" b) a (= "1" a) "1" (= "1" b) "1" :else ["OR" a b]))
(defn gate [a b c-in]
  (let [[a b] (sort [a b])
        a-xor-b (xor a b)]
    [(xor a-xor-b c-in)
     (wire-or (wire-and a-xor-b c-in) (wire-and a b))]))

(def third (comp first next next))


(let [xs (for [i (range 45)] (format "x%02d" i))
      ys (for [i (range 45)] (format "y%02d" i))
      sums (next (reductions (fn [[_ carry] [a b]] (gate a b carry))
                             ["0" "0"]
                             (map vector xs ys)))]
  (def good-gates (into (sorted-map)
                        (zipmap z-keys (concat (map first sums) [(second (last sums))]))))
  (println :whaticameupwith (third good-gates))
  (println :wiring (third wiring))

  ;; 
  )

(defn print-diff! [wire reference]
  (println :diff (meta wire)))

(defn compare-wirings [wire reference]
  (cond (= wire reference)
        true

        (not= (first wire) (first reference))
        (do (print-diff! wire reference) false)

        (and (= (first (second wire)) (first (second reference)))
             (= (first (third wire))  (first (third reference))))
        (do (compare-wirings (second wire) (second reference))
            (compare-wirings (third wire) (third reference)))

        (and (= (first (second wire)) (first (third reference)))
             (= (first (third wire))  (first (second reference))))
        (do (compare-wirings (second wire) (third reference))
            (compare-wirings (third wire) (second reference)))

        :else true))

(doseq [z z-keys]
  (let [factory (wiring z)
        reference (good-gates z)]
    (compare-wirings factory reference)))

; compare zs to wiring

;; z16 z20 z33.
;; z16 z20 z33 tpc mgc (??)


;; z16 + hmk (ellenorizve.)
;; z20 + fhp
;; z33 + fcd.
;; 