(ns advent.day09.marbles)

;; Inspired by https://github.com/Sorceror/aoc2018/blob/master/src/aoc2018/day09.clj

;; marbles 0 to N inc 1

;; the next marble is placed between the 1 and 2 marbles clockwise of the current marble

;; if 23 then
;;   keep the marble that they would have played (the current lowest) and add to score
;;   remove the marble 7 marbles counter-clockwise from the current marble and add to score
;;   then the marble clockwise from the removed marble becomes the current marble


;; (0)
;;  0  (1)
;;  0  (2)  1
;;  0   2   1  (3)
;;  0  (4)  2   1  3


;; circle is the data structure

(defn clockwise [idx num circle]
  ;; return the next idx by rotating
  (mod (+ idx num) (count circle)))

(defn counter-clockwise [idx num circle]
  (mod (- idx num) (count circle)))

(defn insert-after [idx val circle]
  (apply conj (vec (take (inc idx) circle)) val (vec (drop (inc idx) circle))))

(defn remove-at [idx circle]
  ;; return [value at pos
  ;;         new circle
  [(first (drop idx circle))
   (apply conj (vec (take idx circle)) (vec (drop (inc idx) circle)))])

(defn process [num-players last-worth]
  (loop [idx 0
         marble 1
         circle [0]
         player-id 0
         players (vec (repeat num-players 0))]
    ;; let us know that links are still working by printing milestones
    ;; (if (zero? (mod marble 5000)) (println marble))
    (if (= (mod marble 23) 0)
      ;; special 23 case
      (let [new-idx (counter-clockwise idx 7 circle)
            [val new-circle] (remove-at new-idx circle)]
        (recur new-idx (inc marble) new-circle (clockwise player-id 1 players) 
               (assoc players player-id (+ (get players player-id) val marble))))
      ;; else
      (let [pos (clockwise idx 1 circle)
            new-circle (insert-after pos marble circle)]
        (if (> marble last-worth)
          players
          (recur (inc pos) (inc marble) new-circle (clockwise player-id 1 players) players))))))

(comment
  ;; test 
  (apply max (process 9 25))  ;; 32
  )


(defn part1 []
  ;; 491 players; last marble is worth 71058 points
  (apply max (process 491 71058))
  )

(comment
  (part1)   ;;  361466
)


;; ----------------------------------------------------------------------------------------------------
;; Part2
;;
;; The above code takes too long so looking for other ideas

;; (defn part2 []
;;   ;; 491 players; last marble is worth 71058 * 100 (7,105,800) ppoints
;;   (apply max (process 491 (* 100 7o1058)))
;;   )


;; ----------------------------------------------------------------------------------------------------
;; Tried refactoring to remove the loop/recur but this was still too slow


;; (defn init-game [num-players]
;;   {:idx 0
;;    :marble 0
;;    :circle [0]
;;    :player-id 0
;;    :players (vec (repeat num-players 0))}
;; )
;;
;; (defn process [game]
;;   (let [{:keys [idx marble circle player-id players]} game]
;;     (if (= (mod marble 23) 0)
;;       ;; special 23 case
;;       (let [new-idx (counter-clockwise idx 7 circle)
;;             [val new-circle] (remove-at new-idx circle)]
;;         (assoc game :idx new-idx :marble (inc marble) :circle new-circle :player-id (clockwise player-id 1 players) :players (assoc players player-id (+ (get players player-id) val marble))))
;;       ;; else
;;       (let [pos (clockwise idx 1 circle)
;;             new-circle (insert-after pos marble circle)]
;;           (assoc game :idx (inc pos) :marble (inc marble) :circle new-circle :player-id (clockwise player-id 1 players) :players players)))
;;     )
;; )
;;
;; (defn part1 []
;;   (let [game (-> (iterate process (init-game 491))
;;                  (nth (dec 71058)))]
;;     (apply max (:players game)))
;;   )




;; ----------------------------------------------------------------------------------------------------
;; Looked to mfikes for a solution
;; @see https://github.com/mfikes/advent-of-code/blob/master/src/advent_2018/day_09.cljc


;; Correct Answer
;; 2945918550


(defn init-game [players]
  {:players players
   :player  1
   :index   1.0
   :marble  1
   :circle  (sorted-map 0.0 0 1.0 1)
   :scores  (zipmap (map inc (range players)) (repeat 0))})

(defn place-index [circle index]
  (let [[lb ub] (keys (rest (concat (subseq circle >= index) (cycle circle))))]
    (if (< lb ub)
      (/ (+ lb ub) 2.0)
      (inc (ffirst (rseq circle))))))

(defn counter-clockwise-index [circle index n]
  (-> (keys (concat (rsubseq circle <= index) (cycle (rseq circle))))
    (nth n)))

(defn advance [game]
  (let [{:keys [players player index marble circle scores]} game
        marble (inc marble)
        player (inc (mod player players))
        game   (assoc game :marble marble :player player)]
    (if (= (mod marble 23) 0)
      (let [remove-index (counter-clockwise-index circle index 7)
            scores       (update scores player + marble (circle remove-index))
            circle       (dissoc circle remove-index)
            index        (first (first (or (subseq circle > remove-index) circle)))]
        (assoc game :index index :circle circle :scores scores))
      (let [index  (place-index circle index)
            circle (assoc circle index marble)]
        (assoc game :index index :circle circle))
      )))

(defn solve [num-players last-worth]
  (let [game (-> (iterate advance (init-game num-players))
                 (nth (dec last-worth)))]
    (apply max (vals (:scores game)))))


;; Part1 
;; (time (solve 491 71058)
;; "Elapsed time: 345922.048125 msecs"
;; 361466


(defn part2 []
  (time (solve 491 (* 100 71058))))

(comment
  (part2)   ;;   2945918550
)
