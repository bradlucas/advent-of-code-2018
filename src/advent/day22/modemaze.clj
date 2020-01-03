(ns advent.day22.modemaze
  (:require [clojure.set :as set]))

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

