(ns advent.day14.recipe
  (:require [clojure.string :as str]))


;; The following was used and an example to learn how to solve this puzzle
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
  (part1)     ;; 9315164154
)
