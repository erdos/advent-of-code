#!/usr/bin/env bb

(require '[clojure.string :as s])

(defn ->long [s] (Long/valueOf s))

(defn parse-hex [hex]
  (mapcat
   (zipmap "0123456789ABCDEF"
           ["0000" "0001" "0010" "0011"
            "0100" "0101" "0110" "0111"
            "1000" "1001" "1010" "1011"
            "1100" "1101" "1110" "1111"])
   hex))

(def line (slurp "input16.txt"))
(def longline (parse-hex line))

(defn parse-bin [s] (Long/parseLong (apply str s) 2))

(defn paket [line]
  (assert (seq line))
  (let [[version line] (split-at 3 line)
        [type-id line] (split-at 3 line)]
    (if (= type-id [\1 \0 \0])
      (loop [line line, groups [], read-bits 6]
        (let [[last? b c d e & line] line
              groups (into groups [b c d e])]
          (if (= \1 last?)
            (recur line groups (+ read-bits 5))
            [{; :groups groups
              :number (parse-bin groups)
              :type :literal
              :version (parse-bin version)
              :read-bits (+ read-bits 5)}
             line])))
      ;; else operator
      (let [[length-type-id & line] line]
        (if (= \0 length-type-id)
          ;; next 15 bits represent total length in bits
          (let [[len line] (split-at 15 line)]
            [{:subpacket-length-in-bits (parse-bin len)
              :type (parse-bin type-id)
              :read-bits 22
              :version (parse-bin version)}
             line]) ;; 3 + 3 + 1 + 15
          (let [[len line] (split-at 11 line)]
            [{:subpacket-count (parse-bin len)
              :type (parse-bin type-id)
              :read-bits 18
              :version (parse-bin version)}
             line])))))) ;; 3 + 3 + 1 + 11

(defn pakets [line]
  (assert (seq line))
  (let [[p line] (paket line)]
    (or (when (= :literal (:type p))
          [p line])

        (when-let [bits (:subpacket-length-in-bits p)]
          (loop [read-bits 0
                 line      line
                 pkgs      []]
            (if (< read-bits bits)
              (let [[p line] (pakets line)]
                (recur (+ read-bits (:read-bits p))
                       line
                       (conj pkgs p)))
              [(-> p
                   (assoc :subpackets pkgs)
                   (update :read-bits + bits))
               line])))

        (when-let [scount (:subpacket-count p)]
          (loop [scount scount
                 read-bits (:read-bits p)
                 line line
                 pkgs []]
            (if (pos? scount)
              (let [[p line] (pakets line)]
                (recur (dec scount)
                       (+ read-bits (:read-bits p))
                       line
                       (conj pkgs p)))
              [(assoc p :subpackets pkgs :read-bits read-bits) line]))))))

(->> (reduce + (keep :version (tree-seq coll? seq (pakets longline))))
     (println "First answer:"))

(defn eval [p]
  (case (:type p)
    :literal (:number p)
    0 (reduce + (map eval (:subpackets p)))
    1 (reduce * (map eval (:subpackets p)))
    2 (apply min (map eval (:subpackets p)))
    3 (apply max (map eval (:subpackets p)))
    5 (if (> (eval (first (:subpackets p))) (eval (second (:subpackets p)))) 1 0)
    6 (if (< (eval (first (:subpackets p))) (eval (second (:subpackets p)))) 1 0)
    7 (if (= (eval (first (:subpackets p))) (eval (second (:subpackets p)))) 1 0)))

(assert (= 7 (eval (first (pakets (parse-hex "880086C3E88112"))))))
(assert (= 9 (eval (first (pakets (parse-hex "CE00C43D881120"))))))
(assert (= 1 (eval (first (pakets (parse-hex "D8005AC2A8F0"))))))
(assert (= 0 (eval (first (pakets (parse-hex "F600BC2D8F"))))))
(assert (= 0 (eval (first (pakets (parse-hex "9C005AC2F8F0"))))))
(assert (= 1 (eval (first (pakets (parse-hex "9C0141080250320F1802104A08"))))))

(->> (eval (first (pakets longline)))
     (println "Second answer:"))
