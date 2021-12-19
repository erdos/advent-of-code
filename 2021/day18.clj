#!/usr/bin/env bb

(require '[clojure.string :as s] '[clojure.zip :as zip])

(def input (slurp "input18.txt"))
(def lines (vec (s/split-lines input)))
(def vecs (map read-string lines))

(defn magnitude [x]
  (if (number? x)
    x
    (+ (* 3 (magnitude (first x))) (* 2 (magnitude (second x))))))

(defn split-regular [x] (assert (integer? x))
  [(int (/ x 2)) (int (Math/ceil (/ x 2)))])

(defn depth [z] (if (nil? z) -1 (inc (depth (zip/up z)))))

(defn next-leaf [z]
  (when z
    (if-let [zr (zip/right z)]
      (loop [zd zr]
        (if (number? (zip/node zd))
          zd
          (recur (zip/down zd))))
      (recur (zip/up z)))))

(defn prev-leaf [z]
  (when z
    (if-let [zr (zip/left z)]
      (loop [zd zr]
        (if (number? (zip/node zd))
          zd
          (recur (zip/rightmost (zip/down zd)))))
      (recur (zip/up z)))))

(defn explode-4-deep [x]
  (loop [zipper (zip/vector-zip x)]
    (when-not (zip/end? zipper)
      (let [depth (depth zipper)]
        (if (and (= 4 depth) (vector? (zip/node zipper)))
          (let [[left right] (zip/node zipper)]
            (-> (zip/replace zipper 0)
                ((fn [zipper]
                   (if-let [pz (prev-leaf zipper)]
                     (next-leaf (zip/edit pz + left)) ;; return to zero
                     zipper)))
                ((fn [zipper]
                   (if-let [nz (next-leaf zipper)]
                     (zip/edit nz + right)
                     zipper)))
                (zip/root)))
          (recur (zip/next zipper)))))))

(defn split-leftmost-10 [x]
  (if (vector? x)
    (if-let [ls (split-leftmost-10 (first x))]
      [ls (second x)]
      (if-let [rs (split-leftmost-10 (second x))]
        [(first x) rs]))
    (when (>= x 10) (split-regular x))))

(defn reduced-result [x]
  (let [fx (or (explode-4-deep x)
               (split-leftmost-10 x)
               x)]
    (if (= x fx) fx
        (recur fx))))

(defn addition [a b] (reduced-result [a b]))

(println "First answer:" (magnitude (reduce addition vecs)))

(->> (apply max (for [a vecs b vecs :when (not= a b)]
                  (magnitude (addition a b))))
     (time)
     (println "Second answer:"))
