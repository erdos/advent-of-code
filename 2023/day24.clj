(ns day24)

(load-file "common.clj")

(def data
  (->> lines
       (map (partial re-matches #" *(-?\d+), +(-?\d+), +(-?\d+) @ +(-?\d+), +(-?\d+), +(-?\d+)"))
       (map next)
       (map (partial mapv parse-long))))

(println data)

(def data-xy
  (for [[x y _ dx dy _] data] [x y dx dy]))

(defn intersect [[ax ay dax day] [bx by dbx dby]]
  (let [discr (- (* dax dby) (* day dbx))]
    (when-not (zero? discr)
      (let [dx (- bx ax)
            dy (- by ay)
            u (/ (- (* dby dx) (* dbx dy)) discr)
            v (/ (- (* day dx) (* dax dy)) discr)]
        [(+ ax (* u dax)) (+ ay (* u day)) u v]))))

(->> (for [[a & as] (suffixes data-xy)
           b as
           :let [[x y, u v] (intersect a b)]
           :when (and u v (not (neg? u)) (not (neg? v)))
           :when (<= 200000000000000 x 400000000000000)
           :when (<= 200000000000000 y 400000000000000)]
       '(println [u v (double x) (double y)]))
     (count)
     (println :first))

;; Part Two

(->>
 (for [[i [x y z, dx dy dz]] (map-indexed vector data)]
   [(format "%d + t%d * (%d) = x + t%d * dx" x i dx i)
    (format "%d + t%d * (%d) = y + t%d * dy" y i dy i)
    (format "%d + t%d * (%d) = z + t%d * dz" z i dz i)])
 (apply concat)
 (take 10)
 (clojure.string/join ", ")
 (printf "solve([%s]);"))

;; and then enter it into maxima.
