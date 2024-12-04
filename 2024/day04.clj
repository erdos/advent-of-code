(ns day04)

(declare grid)

(load-file "common.clj")

(defn south [[y x]] [(inc y) x])
(defn east [[y x]] [y (inc x)])

(defn se [[y x]] [(inc y) (inc x)])
(defn ne [[y x]] [(dec y) (inc x)])
(defn sw [[y x]] [(inc y) (dec x)])
(defn nw [[y x]] [(dec y) (dec x)])

(def mapper
  (juxt (juxt (comp) (comp south) (comp south south) (comp south south south))
        (juxt (comp) (comp east) (comp east east)    (comp east east east))
        (juxt (comp) (comp se)   (comp se se)        (comp se se se))
        (juxt (comp) (comp ne)   (comp ne ne)        (comp ne ne ne))))

(def coords
  (for [y (range (count grid))
        x (range (count (grid y)))]
    [y x]))

(def get-grid (partial map (partial get-in grid)))

(->> coords
     (mapcat mapper)
     (map get-grid)
     (filter #{(vec "XMAS") (vec "SAMX")})
     (count)
     (println 'First))

(defn xm? [pt]
  (and (#{(vec "MAS") (vec "SAM")} (get-grid ((juxt se identity nw) pt)))
       (#{(vec "MAS") (vec "SAM")} (get-grid ((juxt sw identity ne) pt)))))

(->> coords
     (filter xm?)
     (count)
     (println 'Second))