(ns advent.day23.nanobot
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [clojure.java.io :as io]
            ))

;; https://github.com/pgorczak/adventofcode-clj/blob/master/src/aoc2018/day_23.clj

;; Input data
;; 
;; pos=<-16209987,47027970,47490542>, r=98801023

(def input-file "resources/day23/input.txt")

(defn data-line [s]
  (->> (re-matches #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" s)
       rest
       (map read-string)
       (zipmap [:x :y :z :r])))

(defn load-input []
  (let [lines (-> input-file
                  slurp
                  str/split-lines)]
    (map data-line lines)))

(defn strongest [m]
  (apply max-key :r m))

(defn dist [a b]
  (+ (Math/abs (- (:x a) (:x b)))
     (Math/abs (- (:y a) (:y b)))
     (Math/abs (- (:z a) (:z b)))))


(defn in-range
  "Is b within range of a (the strongest)"
  [a b]
  (<= (dist a b) (:r a)))


(defn part1 []
  (let [bots (load-input)
        strong-bot (strongest bots)]
    (->> (filter #(in-range strong-bot %) bots)
         count)))


(comment
  (part1)
  ;; => 573
  )

