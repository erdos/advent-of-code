(ns day09)


(load-file "common.clj")

(def char->int (comp parse-long str))

(def data-vec-vec
  (time (vec ((fn f [tail mode id]
                (when-let [[t & tail] (seq tail)]
                  (cons (repeat (char->int t) (if mode id \.))
                        (lazy-seq (f tail (not mode) (if mode (inc id) id))))))
              data true 0))))

(def data-vec (vec (apply concat data-vec-vec)))

;(println data-vec)
(println "Input length:" (count data-vec))

(defn checksum [data-vec]
  (transduce (map-indexed (fn [idx v] (if (= v \.) 0 (* idx v))))
             + data-vec))

(->> (loop [data-vec data-vec
            idx-start 0
            idx-end (dec (count data-vec))]
       (cond (>= idx-start idx-end)
             data-vec

             (not= \. (data-vec idx-start))
             (recur data-vec (inc idx-start) idx-end)

             (= \. (data-vec idx-end))
             (recur data-vec idx-start (dec idx-end))

             :else
             (-> data-vec
                 (assoc idx-start (data-vec idx-end) idx-end \.)
                 (recur (inc idx-start) (dec idx-end)))))
     (checksum)
     (println 'First)
     (time))

;(println data-vec-vec)

;; could optimize this one by not seeking from first element every time.
(defn find-leftmost-space [data-vec-vec maximum]
  (let [length (count (data-vec-vec maximum))]
    (loop [idx 1]
      (cond (>= idx maximum)                        nil ;; not found
            (<= length (count (data-vec-vec idx)))  idx
            :else                                   (recur (+ 2 idx))))))

(defn ->free-block [size]
  (repeat size \.))

; (def ->free-block (memoize ->free-block))

;; merge 3 blocks to one free space
(defn make-free [data-vec-vec block-idx]
  (-> (subvec data-vec-vec 0 (dec block-idx))
      (conj (->free-block (+ (count (data-vec-vec block-idx))
                             (count (get data-vec-vec (dec block-idx) []))
                             (count (get data-vec-vec (inc block-idx) [])))))
      (into (drop (+ 2 block-idx) data-vec-vec))))
;(println :make-free (make-free [[\. \.] [1 2 3] [\.] [3 4]] 3))

(defn alloc-block [data-vec-vec free-idx block]
  (-> (vec (take free-idx data-vec-vec))
      (conj []
            block
            (vec (drop (count block) (data-vec-vec free-idx))))
      (into (drop (inc free-idx) data-vec-vec))))
; (println (alloc-block [[\. '.] [1 2] [\. \. \.] [3 4] [\. \.]] 2 [:a :b :c]))

(->> (loop [data-vec-vec    (vec data-vec-vec)
            rightmost-block (dec (count data-vec-vec))]
       (if (>= 0 rightmost-block)
         data-vec-vec
         (if-let [space-idx (find-leftmost-space data-vec-vec rightmost-block)]
           (recur (-> data-vec-vec
                      (alloc-block space-idx (data-vec-vec rightmost-block))
                      (make-free (+ 2 rightmost-block)))
                  rightmost-block)
           (recur data-vec-vec (- rightmost-block 2)))))
     (apply concat)
     (vec)
     (checksum)
     (println 'Second)
     (time))
