(ns advent.day14.recipe
  (:require [clojure.string :as str]))


;; See the following link to a repo which was used to help solve this puzzle
;; https://github.com/baritonehands/advent-of-code-2018/blob/master/src/aoc/dec14.clj


(def input 505961)

;; scoreboard

;; :recipes [recipes]
;; :elf1 idx1
;; :elf2 idx2

;; start with [3 7]
;; e1 = 0
;; e2 = 1

;; step
;; add e1 and e2 recipes
;; 

(defn string-digits
  "For a given string reprentation of a number return the list of digits it contains.
  For example, `10` is returned as [1 0]"
  [s]
  (mapv (fn [c] (- (int c) (int \0))) s))

(defn step [[e1 e2 recipes]]
  (let [e1v (recipes e1)
        e2v (recipes e2)
        score (+ e1v e2v)
        new-recipes (into recipes (string-digits (str score)))]
    ;; return new e1 and e2 positions and new-recipes
    [(mod (+ e1 (inc (new-recipes e1))) (count new-recipes))
     (mod (+ e2 (inc (new-recipes e2))) (count new-recipes))
     new-recipes]))

(defn part1 []
  (let [input input
        input-string (string-digits (str input))]
    (->> (iterate step [0 1 [3 7]])
         (drop-while (fn [[_ _ recipes]]
                       (< (count recipes) (+ input 10))))
         (take 10)
         (last)
         (last)
         (drop input)
         (take 10)
         (str/join "")
         (read-string))))


(comment
  (time (part1))
  ;; "Elapsed time: 746.764866 msecs"
  ;; 9315164154
)


;; ----------------------------------------------------------------------------------------------------
;; Part 2

;; See the following link to a repo which was used to help solve this puzzle
;; https://github.com/Average-user/adventofcode-clj-2018/blob/master/src/adventofcode_clj_2018/day14.clj


(defn solve [input]
  (loop [e1 0
         e2 1
         rs [3 7]]
    (let [e1-move (inc (nth rs e1))
          e2-move (inc (nth rs e2))
          sum    (+ (nth rs e1) (nth rs e2))

          rs'    (if (<= 10 sum)
                   (into rs [(quot sum 10) (rem sum 10)])
                   (conj rs sum))
          rsc    (count rs')
          cond1  (and (<= 6 rsc) (= input (subvec rs' (- rsc 6))))
          cond2  (and (<= 7 rsc) (= input (subvec rs' (- rsc 7) (+ 6 (- rsc 7)))))]
      (cond cond1 (subvec rs' 0 (- rsc 6))
            cond2 (subvec rs' 0 (- rsc 7))
            :else (recur (rem (+ e1 e1-move) rsc) (rem (+ e2 e2-move) rsc) rs')))))

(defn part2 []
  (count (solve (string-digits (str input)))))

(comment
  (time (part-2))  
  ;; "Elapsed time: 17192.560481 msecs"
  ;; 20231866
)
