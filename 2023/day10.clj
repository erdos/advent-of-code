(ns day10)

(load-file "common.clj")

(defn parse-line [line]
  (mapv parse-long (.split line " ")))

(defn north [[i j]] [(dec i) j])
(defn south [[i j]] [(inc i) j])
(defn east  [[i j]] [i (inc j)])
(defn west  [[i j]] [i (dec j)])

(def coord->cell
  (into {} (for [[i row] (map-indexed vector grid)
                 [j cell] (map-indexed vector row)]
            [[i j] cell])))

(def start-loc (some (fn [[loc cell]] (when (= \S cell) loc)) coord->cell))
(def start-dir :south) ;; good enough

(defn reachable [loc]
  (let [north [(dec (loc 0)) (loc 1)]
        south [(inc (loc 0)) (loc 1)]
        west [(loc 0) (dec (loc 1))]
        east [(loc 0) (inc (loc 1))]]
    (case (coord->cell loc)
        \S (list south east) ;; jobbra es le
        \- (filter coord->cell (list west east))
        \| (filter coord->cell (list north south))
        \L (filter coord->cell (list north east))
        \7 (filter coord->cell (list west south))
        \F (filter coord->cell (list south east))
        \J (filter coord->cell (list north west))
        \. (filter (comp #{\.} coord->cell) (list north south east west)) ;; for second part
        )))

(->> [#{} #{start-loc}]
     (iterate (fn [[reached new]]
                [(into reached new)
                 (into #{} (remove reached (mapcat reachable new)))]))
     (partition 2 1)
     (take-while (partial apply not=))
     count dec
     (println "First"))

(defn floodfill [start-locs]
  (assert (set? start-locs))
  (first (fixpt (fn [[visited new]]
                  [(into visited new) (set (remove visited (mapcat reachable new)))])
                [#{} start-locs])))

(defn step [loc dir]
  (condp = [(coord->cell loc) dir]
    [\S :east] [(east loc) :east]     [\S :west] [(west loc) :west]
    [\S :south] [(south loc) :south]  [\S :north] [(north loc) :north]

    [\- :east] [(east loc) :east]
    [\- :west] [(west loc) :west]

    [\| :north] [(north loc) :north]
    [\| :south] [(south loc) :south]

    [\L :south] [(east loc) :east]
    [\L :west]  [(north loc) :north]

    [\J :south] [(west loc) :west]
    [\J :east]  [(north loc) :north]

    [\F :north] [(east loc) :east]
    [\F :west]  [(south loc) :south]

    [\7 :north] [(west loc) :west]
    [\7 :east]  [(south loc) :south]))


;; returns list of locs given to my right side
(defn right-side [loc dir]
 (condp = [(coord->cell loc) dir]
    [\S :east]  (list (south loc)) ;; vizszintesen jobbra
    [\S :south] (list (west loc)) ;; fuggolegesen le
    [\S :north] (list (east loc)) [\S :west] (list (north loc))

    [\- :east] (list (south loc))
    [\- :west] (list (north loc))

    [\| :north] (list (east loc))
    [\| :south] (list (west loc))

    [\L :south] (list (west loc) (south loc))
    [\L :west]  (list)

    [\J :south] (list)
    [\J :east]  (list (south loc) (east loc))

    [\F :north] (list)
    [\F :west]  (list (north loc) (west loc))

    [\7 :north] (list (east loc) (north loc))
    [\7 :east]  (list)))

(def path (promise))
(loop [loc start-loc
       dir start-dir
       visited []]
  (let [[next-loc next-dir] (step loc dir)]
    (if (= start-loc next-loc)
      (do (deliver path (conj visited loc)))
      (recur next-loc next-dir
             (conj visited loc)))))

;; REDEFINE so only path is there.
(def grid
  (let [seen? (set @path)]
     (mapv-indexed (fn [i row] (mapv-indexed (fn [j c] (if (seen? [i j]) c \.)) row)) grid)))
(def coord->cell
  (into {} (for [[i row] (map-indexed vector grid) [j cell] (map-indexed vector row)] [[i j] cell])))

(def enclosed-start-locs (promise))
(loop [loc start-loc
       dir start-dir
       locs-to-the-right #{}]
  (let [[next-loc next-dir] (step loc dir)]
    (if (= start-loc next-loc)
       (deliver enclosed-start-locs locs-to-the-right)
      (recur next-loc next-dir
             (into locs-to-the-right (filter (comp #{\.} coord->cell) (right-side loc dir)))))))

; (println :enclosed @enclosed-start-locs)
(println "Second" (count (floodfill @enclosed-start-locs)))

(println "New grid")
(doseq [g grid] (println (apply str g)))
