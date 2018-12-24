(ns advent.day15.combat
  (:require [clojure.string :as str]))


(def input "resources/day15/input.txt")

(defn read-input []
  (str/split-lines (slurp input)))

(defn test-input []
  ["#########"
   "#G..G..G#"
   "#.......#"
   "#.......#"
   "#G..E..G#"
   "#.......#"
   "#.......#"
   "#G..G..G#"
   "#########"])


;; Result for test-input2
;; the number of full rounds that were completed is 47, and the sum of the hit points of all remaining units is 200+131+59+200 = 590. 
;; From these, the outcome of the battle is 47 * 590 = 27730
(defn test-input2 []
  ["#######"
   "#.G...#"
   "#...EG#"
   "#.#.#G#"
   "#..G#E#"
   "#.....#"
   "#######"])

;; ----------------------------------------------------------------------------------------------------
;; #   == wall
;; .   == open
;; (G) == Goblin
;; (E) == Elf


;; Combat is in rounds where each unit (G,E) takes a turn.
;; Each unit tries to move into range of an enemy if it isn't aleady
;; and then attack if it is range.

;; Turns are chosen in reading order (top-bottom, left-right)


;; Each Turn
;;
;; MOVE
;; - Identify targets. If none, combat ends
;;
;; - Identify In Range
;;   - Spaces which are adjacent to targets
;;
;; - Reachable
;;   - Those spaces which have a path to the unit
;;
;; - Nearest
;;   - Choose the nearest by reading-order
;;
;; Then take a single step toward the chosen square only the shortest path
;; If there is a multiple choice then use reading-order
;;
;; ATTACK
;; - Identify all target a uniw it adjacent to
;;
;; - Choose the arget with the fewest hit points
;;   - If there is a tie then break the tie by reading order
;;
;; - Damage is reduce target's poser by the unit's attach power
;;   - If this reduces the targets hit point to 0 or less then the target dies
;;     and the cel becomes a .
;;
;; 
;; INITIALIZATION
;; - Each Goblin and Elf has 3 attack power and 200 hit points.
;;
;; OUTCOME
;; - Number of full rounds that were completed (not counting the round where combat ends)
;; - Multpley by the sum of the hit points of all remaining units at the moment combat ends
;; 


;; @see https://github.com/baritonehands/advent-of-code-2018/blob/master/src/aoc/dec15.clj

(defn load-input 
  "Expecting list of strings"
  [input]


)
