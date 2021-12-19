#!/usr/bin/env bb

(def xfrom 179) (def xto 201) (def yfrom -109) (def yto -63)

(->> (for [yvel (range 1 2000)]
       (loop [m     0
              yvel' yvel
              y     0]
         (cond (< y yfrom) nil
               (<= yfrom y yto) [m yvel']
               (> y yto) (recur (max m y) (dec yvel') (+ y yvel')))))
     (keep first)
     (apply max)
     (println "First answer:"))

(defn drag [x] (cond (pos? x) (dec x) (neg? x) (inc x) :else 0))

(def valid-xvels
  (for [xvel (range 1 (inc xto))
        :when (loop [xvel (int xvel)
                     x     0]
                (cond (> x xto) false
                      (<= xfrom x xto) true
                      (zero? xvel) false
                      :else     (recur (drag xvel) (+ x xvel))))]
    xvel))

(def valid-yvels
  (for [yvel (range yfrom (inc (- yfrom)))
        :when (loop [yvel (int yvel)
                     y     0]
                (cond (< y yfrom) false
                      (<= yfrom y yto) true
                      :else     (recur (dec yvel) (+ y yvel))))]
    yvel))

(->> (for [xvel valid-xvels
           yvel valid-yvels
           :when (loop [xvel (int xvel), yvel (int yvel)
                        x 0, y 0]
                   (cond (> x xto) false
                         (< y yfrom) false
                         (and (<= xfrom x xto) (<= yfrom y yto)) true
                         :else   (recur (drag xvel) (dec yvel) (+ x xvel) (+ y yvel))))]
       1)
     (count)
     (println "Second answer:"))
