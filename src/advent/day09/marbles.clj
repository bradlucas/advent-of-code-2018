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
    (if (zero? (mod marble 5000)) (println marble))
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



;; 365659  - wrong

