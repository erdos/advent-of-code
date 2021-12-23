#!/usr/bin/env bb

(set! *warn-on-reflection* true)

(require '[clojure.string :as s])

(defn ->long [x] (Long/valueOf ^String x))
(defn third [x] (nth x 2))

(def start (mapv vec (s/split-lines (slurp "input23.txt"))))

(def desired (mapv vec ["#############"
                        "#...........#"
                        "###A#B#C#D###"
                        "  #A#B#C#D#"
                        "  #A#B#C#D#" ;; part 2
                        "  #A#B#C#D#" ;; part 2
                        "  #########"]))

(def costs (zipmap "ABCD" [1 10 100 1000]))
(def letter->column (zipmap "ABCD" [3 5 7 9]))

(defn safe+ [a b]
  (cond (= a Long/MAX_VALUE) Long/MAX_VALUE
        (= b Long/MAX_VALUE) Long/MAX_VALUE
        :else (+ a b)))

(defn cost [state action i j]
  ;(println :action action i j) (run! println state) (println)
  (when action
    (assert (costs (get-in state [i j]))))
  (if (= state desired)
    0
    (case action

      (:awaken-move-left :awaken-move-right) ;; i=1 moving
      (->
       (let [j' (if (= :awaken-move-left action) (dec j) (inc j))]
             (if-not (= \. (get-in state [1 j']))
               Long/MAX_VALUE
               (let [letter (get-in state [1 j])
                     new-state (-> state (update 1 assoc j \. j' letter))]
                 (safe+ (costs letter)
                        (cost new-state action 1 j')))))

           ;; i can stop on these places if i want
           (cond-> (#{1 2 4 6 8 10 11} j) (min (cost state nil nil nil))))

      :awaken-move-up
      (if (= i 1)
        (min (cost state :awaken-move-left i j)
             (cost state :awaken-move-right i j))
        ;; if field above me is free
        (let [i' (dec i)
              letter (get-in state [i j])
              new-state (-> state (assoc-in [i j] \.) (assoc-in [i' j] letter))]
          (safe+ (costs letter)
                 (cost new-state :awaken-move-up i' j))))

      ;; balra/jobbra megy es keresi a helyet. megindul lefele amikor tud. megall ha jo helyen van.
      :resting
      (let [letter (get-in state [i j])
            my-column (letter->column letter)
            j' (cond (< j my-column) (inc j) (> j my-column) (dec j) :else j)
            i' (if (= j my-column) (inc i) i)]
        (if (not= \. (get-in state [i' j']))
          (if (= 1 i)
            Long/MAX_VALUE ;; cannot move left/right because of others
            (cost state nil nil nil))
          (let [new-state (-> state
                              (assoc-in [i j] \.)
                              (assoc-in [i' j'] letter))]
            (safe+ (costs letter) (cost new-state :resting i' j')))))

      nil
      (apply min
             Long/MAX_VALUE
             (concat
              ;; elso sor: kit tudok elinditani balra-jobbra?
              (for [j (range 1 12)
                    :let [letter (get-in state [1 j])]
                    :when (not= \. letter)
                    ;; es akinek a soraban nem all masik betu
                    :let [my-column (letter->column letter)]
                    :when (every? #{\. letter} (for [c [1 2 3 4 5]] (get-in state [c my-column])))]
                (cost state :resting 1 j))

              ;; masodik harmadik sor: ki folott nincs senki es rossz sorban van?
              (for [i [2 3 4 5] j [3 5 7 9]
                    ;; minden mezo szabad folotte
                    :when (every? #{\.} (for [i (range 1 i)] (get-in state [i j])))
                    :let [letter (get-in state [i j])]
                    :when (costs letter)
                    :when (or (not= (letter->column letter) j)
                              (not-every? #{letter} (for [i (range i 6)] (get-in state [i j]))))]
                (cost state :awaken-move-up i j)))))))

(def cost (memoize cost))

(println "Second answer:" (cost start nil nil nil))

;; ...........
;;   D C B C
;;   D A A B
