(ns day15)

(def dir-diff {\< [0 -1] \> [0 1] \v [1 0] \^ [-1 0]})

(def data     (slurp *in*))
(def grid     (-> (.split data "\n\n") first (.split "\n") vec (->> (mapv vec))))
(def commands (->> (.split data "\n\n") second (filter dir-diff) vec))

(defn robot-loc [grid]
  (first (for [i (range (count grid))
               j (range (count (first grid)))
               :when (#{\@} (get-in grid [i j]))]
           [i j])))

(defn add-vec [a b] (mapv + a b))

;; lazy seq of next positions in direction
(defn next-locs [loc dir]
  (iterate (partial add-vec (dir-diff dir)) loc))

;; update grid
(defn move [grid dir]
  (let [loc      (robot-loc grid)
        next-loc (mapv + loc (dir-diff dir [0 0]))]
    (cond (not (dir-diff dir)) grid ;; garbage in input

          (= \. (get-in grid next-loc))
          (-> grid (assoc-in loc \.) (assoc-in next-loc \@))

          (= \. (first (remove #{\O \@} (map (partial get-in grid) (next-locs loc dir)))))
          (let [eor (first (drop-while (comp #{\O \@} (partial get-in grid)) (next-locs loc dir)))]
            (-> grid
                (assoc-in loc \.)
                (assoc-in next-loc \@)
                (assoc-in eor \O)))

          :else grid)))

(defn print-map [grid]
  (doseq [line grid]
    (println (apply str line)))
  grid)

(defn score [grid]
  (->> grid
       (keep-indexed (fn [y row] (keep-indexed (fn [x c] (when (#{\O \[} c) (+ x (* 100 y)))) row)))
       (apply concat)
       (reduce +)))

(->> (reductions move grid commands)
     (map print-map)
     last
     score
     (println 'First))

(def new-grid
  (mapv (fn [row] (vec (mapcat (fn [c] (case c \# "##" \O "[]" \. ".." \@ "@.")) row))) grid))

(defn push-up [grid loc]
  (assert (= \[ (get-in grid loc)))
  ;(println :push-up!)
  (let [loc-right (mapv + loc [0 1])
        loc-1     (mapv + loc [-1 -1])
        loc-up    (mapv + loc [-1 0])
        loc+1     (mapv + loc [-1 1])]
    (case [(get-in grid loc-up) (get-in grid loc+1)]
      [\. \.]
      (-> grid
          (assoc-in loc \.) (assoc-in loc-right \.) ;; clear current pos 
          (assoc-in loc-up \[) (assoc-in loc+1 \]))

        ;; try push up 
      [\[ \]] (some-> grid (push-up loc-up) (recur loc))
      [\. \[] (some-> grid (push-up loc+1) (recur loc))
      [\] \.] (some-> grid (push-up loc-1) (recur loc))
      [\] \[] (some-> grid (push-up loc-1) (push-up loc+1) (recur loc))
      nil)))

; (println :push-up) (print-map (push-up (mapv vec ["....#" "[][]#" ".[].#"]) [2 1]))

(defn push-down [grid [y x]]
  (assert (= \[ (get-in grid [y x])))
  (some-> grid rseq vec (push-up [(- (count grid) 1 y) x]) rseq vec))

; (print-map (push-down (mapv vec [".[]." "[][]" "...."]) [0 1]))

(defn push-right [grid loc] ;; OK
  ;(assert (= \[ (get-in grid loc)))
  (let [stack (map (partial get-in grid)
                   (iterate (partial add-vec [0 1]) loc))]
    (when (= \. (first (remove #{\[ \] \@} stack)))
      (let [stack-len (count (take-while #{\[ \] \@} stack))]
          ;; move whole row.
        (assert (even? stack-len))
        (-> grid
            (update (loc 0)
                    (fn [row] (vec (concat (take (loc 1) row) [\.] (subvec row (loc 1) (+ (loc 1) stack-len))
                                           (drop (+ (loc 1) stack-len 1) row))))))))))

;(println :! (push-right [(vec "##.[][]#.")] [0 3])) ;; nil
;(println :! (push-right [(vec "##.[][]..#")] [0 3])) ;; ##..[][].#
;(println :! (push-right [(vec "##.[].[].#")] [0 3])) ;; ##..[][].#

(defn push-left [grid loc] ;; OK
  (assert (= \] (get-in grid loc)) (str "!!" (get-in grid loc)))
  (some-> grid
          (update     (loc 0)  (comp vec rseq))
          (push-right [(loc 0) (- (count (grid 0)) 1 (loc 1))])
          (update     (loc 0)  (comp vec rseq))))

;(println :push-left (push-left [(vec "##.[][].##")] [0 6])) ;; ##[][]..##
;(println :push-left (push-left [(vec "###[][]..#")] [0 6])) ;; nil

(defn move2 [grid dir]
  (let [loc      (robot-loc grid)
        next-loc (mapv + loc (dir-diff dir [0 0]))]
    (case [dir (get-in grid next-loc)]

      ;; move left but empty
      [\< \.] (-> grid (assoc-in loc \.) (assoc-in next-loc \@))
      [\> \.] (-> grid (assoc-in loc \.) (assoc-in next-loc \@))
      [\v \.] (-> grid (assoc-in loc \.) (assoc-in next-loc \@))
      [\^ \.] (-> grid (assoc-in loc \.) (assoc-in next-loc \@))

      ;; move left but there is a box
      [\< \]] (some-> (push-left grid next-loc)                  (recur dir))
      [\> \[] (some-> (push-right grid next-loc)                 (recur dir))

      ;; push down
      [\v \[] (some-> (push-down grid next-loc)                  (recur dir))
      [\v \]] (some-> (push-down grid (add-vec next-loc [0 -1])) (recur dir))

      ;; push up
      [\^ \[] (some-> (push-up grid next-loc)                    (recur dir))
      [\^ \]] (some-> (push-up grid (add-vec next-loc [0 -1]))   (recur dir))

      ;; else when block
      nil)))

(->> (reductions (fn [g d] (or (move2 g d) g)) new-grid commands)
     (map print-map)
     last
     (print-map)
     score
     (println 'Second))
