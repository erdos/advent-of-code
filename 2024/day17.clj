(ns day17)

(defn pow2 [a] (long (Math/pow 2 a)))
(defn mod8 [a] (mod a 8))

(def output (atom ""))

(defn run-code [program a b c]
  (loop [a a, b b, c c, ins-ptr 0]
    (if (>= ins-ptr (count program))
      (println "Done" [a b c] (apply str @output))
      (let [op      (program ins-ptr)
            operand (program (inc ins-ptr))
            literal operand
            combo   (delay (case operand
                             (0 1 2 3) operand
                             4 a 5 b 6 c))]
        (case op
          0 ; adv
          (recur (long (quot a (pow2 @combo))) b c (+ 2 ins-ptr))

          1; bxl
          (recur a (bit-xor b literal) c (+ 2 ins-ptr))

          2 ; bst 
          (recur a (mod @combo 8) c (+ 2 ins-ptr))

          3 ; jnz
          (recur a b c (if (zero? a) (+ 2 ins-ptr) literal))

          4 ; bxc 
          (recur a (bit-xor b c) c (+ 2 ins-ptr))

          5 ; out
          (do (println (mod @combo 8))
              (swap! output str "," (mod @combo 8))
              (recur a b c (+ 2 ins-ptr)))

          6 ;; bdv: like a but stores in b register
          (recur a (long (quot a (pow2 @combo))) c (+ 2 ins-ptr))

          7 ;; cdv 
          (recur a b (long (quot a (pow2 @combo))) (+ 2 ins-ptr)))))))

(defn eval-symbolic [program]
  (reduce (fn [[a b c out] [operator operand]]
            (let [literal operand
                  combo   (fn [] (case operand
                                   (0 1 2 3) operand
                                   4 a
                                   5 b
                                   6 c))]
              (case operator
                0 [`(~'quot ~a (~'pow2 ~(combo))) b c out]
                1 [a `(~'bit-xor ~b ~literal)    c out]
                2 [a `(~'mod8 ~(combo))         c out]
                3 (assert false)
                4 [a `(~'bit-xor ~b ~c)        c out]
                5 [a b c (conj out `(~'mod8 ~(combo)))]
                6 [a `(~'quot ~a (~'pow2 ~(combo))) c out]
                7 [a b `(~'quot ~a (~'pow2 ~(combo))) out])))
          ['a 0 0 []]
          (partition 2 program)))

(println (eval-symbolic [2,4,1,3,7,5,0,3,4,1,1,5,5,5]))

(defn output-at [a]
  (mod8 (bit-xor (bit-xor (bit-xor (mod8 a) 3)
                          (quot a (pow2 (bit-xor (mod8 a) 3))))
                 5)))

(defn next-a [a] (quot a (pow2 3)))
(defn combine-a [a' i] (+ i (* a' 8)))

;; list of next a values that produce this output.
(defn solve-for-a [[out & outs]]
  (if-not out
    #{0}
    (let [a'-values (solve-for-a outs)]
      (set (for [a' a'-values
                 i (range 8)
                 :let [a (combine-a a' i)]
                 :when (= out (output-at a))]
             a)))))

(def program [2,4,1,3,7,5,0,3,4,1,1,5,5,5,3,0])
(println 'Part2 (apply min (solve-for-a program)))

(println :testing (run-code program 236581108670061 0 0))
