(ns advent.day22.modemaze
  (:require [clojure.pprint :as pp]
            [clojure.set :as set]))

;; The following was helped by https://github.com/borkdude/advent-of-cljc/blob/master/src/aoc/y2018/d22/iamdrowsy.cljc#L18


;; My input
(def depth 8112)
(def target [13 743])


;; The region at 0,0 (the mouth of the cave) has a geologic index of 0.
;; The region at the coordinates of the target has a geologic index of 0.
;; If the region's Y coordinate is 0, the geologic index is its X coordinate times 16807.
;; If the region's X coordinate is 0, the geologic index is its Y coordinate times 48271.
;; Otherwise, the region's geologic index is the result of multiplying the erosion levels of the regions at X-1,Y and X,Y-1.

(defn geo-index [cave-map target-x target-y x y]
  (cond (and (zero? x) (zero? y)) 0
        (and (= target-x x) (= target-y y)) 0
        (zero? x) (* y 48271)
        (zero? y) (* x 16807)
        :else (* (cave-map [(dec x) y]) (cave-map [x (dec y)]))))

;; A region's erosion level is its geologic index plus the cave system's depth, all modulo 20183.
(defn erosion-level [depth cave [target-x target-y] [x y]]
  (mod (+ depth (geo-index cave target-x target-y x y)) 20183))

;; If the erosion level modulo 3 is 0, the region's type is rocky.
;; If the erosion level modulo 3 is 1, the region's type is wet.
;; If the erosion level modulo 3 is 2, the region's type is narrow.
(defn map-types [m]
  (map (fn [v] (mod v 3)) (vals m)))

(defn build-map [depth target [max-x max-y]]
  (reduce (fn [cave coords]
            (assoc cave coords (erosion-level depth cave target coords)))
          {}
          (for [x (range (inc max-x))
                y (range (inc max-y))]
            [x y])))

(defn part1 []
  (reduce + (map-types (build-map depth target target))))




;; ----------------------------------------------------------------------------------------------------
;; Part 2
;; Again see https://github.com/borkdude/advent-of-cljc/blob/master/src/aoc/y2018/d22/iamdrowsy.cljc#L18 

;; Start at 0,0 with a torch and climbing gear

;; Tools => climbing, torch

;; 0 == rocky =>  climbing, or torch;    you can not use neither (you have to use climbing or torch)
;; 1 == wet   =>  climbing, or neither;  you can not  use the torch
;; 2 == narrow => torch or neither;      you can not use climbing

;; 
;; Movements are up, down, left or right (no diagonal)


;; Can only move to a new regtion is you have the proper tool
;; Moving to an adjacent region takes 1 minute
;; Switching tools takes 7 minutes

;; Target is always rocky

;; Diagram
;; rocky => .
;; wet => =
;; narrow => |

(defn build-type-map
  "Convert the erosion map to one with type values.

[[17 730] 17033]

=>

[[17 730] 2]
"
  [m]
  (map (fn [[a b]] [a (mod b 3)]) m))


(defn all-valid
  " Build set of valid position tool possiblities
    "
  [type-map]
    (reduce (fn [all [[x y] t]]
                 (case t 0 (conj all [x y :torch][x y :climbing])
                         1 (conj all [x y :none] [x y :climbing])
                         2 (conj all [x y :none] [x y :torch])))
               #{}
               type-map))

(defn switch-tool
  "For a given tool return the other choices"
  [[x y t]]
  (map (fn [t] [x y t]) (disj #{:none :torch :climbing} t)))

(defn move-single
  "Return the next positions"
  [[x y t]]
  #{[(inc x) y t] [(dec x) y t] [x (inc y) t] [x (dec y) t]})

(defn move [current]
  (reduce set/union (map move-single current)))

(defn solve-2 [depth target]
  (let [t (conj target :torch)
        ;; (* 7 %) is an heuristic to generate a cave big enough we would never leave it
        type-map (build-type-map (build-map depth target (map #(* 7 %) target)))
        all-valid (all-valid type-map)]
    (loop [time 1
           not-visited all-valid
           reachable {0 [[0 0 :torch]]}]
      (let [next (set/intersection (into (move (reachable (dec time) []))
                                         (mapcat switch-tool (reachable (- time 7) [])))
                                   not-visited)]
        (if (next t)      ;; at target
          time
          (recur (inc time) (set/difference not-visited next) (assoc reachable time next)))))))


(defn part2 []
  (solve-2 depth target))


(comment
  (part2)
  ;; => 1010
  )
