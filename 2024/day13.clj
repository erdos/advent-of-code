(ns day13)

;; Each entry contains a [ax ay bx by px py] tuple so we just have to just
;; solve (i)   N*ax + M*bx = px
;;   and (ii)  N*ay + M*by = py
;      to get  3*N + M
;; where N = number of pushes to A button
;;   and M = number of pushes to B button
;;
;; 1. Reordering (i) gets us:  M = (px - N*ax) / bx
;; 2. Sobstituting into (ii): py = N*ay + (px-N*ax) / bx*by
;; 3. Expand the subtraction: py = N*ay + px/bx*by  - N*ax/bx*by
;; 4. Move right:  py - px/bx*by = N*ay   - N*ax/bx*by
;; 5. Reorg again:             N = (py - px/bx*by) / (ay - ax/bx*by)
;;
;; Note, we only care about integer number of button presses.

(defn solve-machine [[ax ay bx by px py]]
  (let [n (/ (- py (-> px (/ bx) (* by)))
             (- ay (-> ax (/ bx) (* by))))
        m (-> px (- (* n ax)) (/ bx))]
    (when (and (integer? n) (integer? m))
      (long (+ (* n 3) m)))))

(def input
  (for [[_ & coords]
        (re-seq #"Button A: X([+-]\d+), Y([+-]\d+)\nButton B: X([+-]\d+), Y([+-]\d+)\nPrize: X=(\d+), Y=(\d+)"
                (slurp *in*))]
    (mapv parse-long coords)))

(println 'First (transduce  (keep solve-machine) + input))

(->> (for [line input]
       (-> line (update 4 + 10000000000000) (update 5 + 10000000000000)))
     (transduce (keep solve-machine) +)
     (println 'Second))
