(ns day02)

(load-file "common.clj")

;; watch -n 1 "xclip -selection c | bb day01.clj"

(def rules (map #(str/split % #" ") lines))

(def m {"X" :rock
        "Y" :paper
        "Z" :scissors
        "A" :rock
        "B" :paper
        "C" :scissors})

(defn score [me opp]
  (case [me opp]
    [:paper :rock] 6
    [:rock :scissors] 6
    [:scissors :paper] 6

    [:paper :paper] 3
    [:rock :rock] 3
    [:scissors :scissors] 3
    ;0
    [:rock :paper] 0
    [:scissors :rock] 0
    [:paper :scissors] 0
    ))

(defn myact [letter opp]
  (case [letter opp]
    ["X" :rock] :scissors
    ["X" :paper] :rock
    ["X" :scissors] :paper

    ["Y" :rock] :rock ["Y" :paper] :paper ["Y" :scissors] :scissors

    ["Z" :rock] :paper ["Z" :paper] :scissors ["Z" :scissors] :rock
  ))

(println
(reduce (fn [sum [opponent whatdo]]
          (let [opponent (m opponent)
                mine     (myact whatdo opponent)
                ss       (case mine :rock 1 :paper 2 :scissors 3)
                bb       (score mine opponent)]
            (+ sum ss bb)))
  0
  rules)
)