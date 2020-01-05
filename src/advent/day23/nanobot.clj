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


;; ----------------------------------------------------------------------------------------------------
;; Part 2


;; @see https://github.com/thegeez/clj-advent-of-code-2018/blob/master/src/advent2018/core3.clj
;; and
;; h/t https://www.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecdqzdg/

(defn load-bots [in]
  (into {}
        (map-indexed (fn [id line]
                       (let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" line)
                             [x y z r] (map #(Long/parseLong %) [x y z r])]
                         [id [x y z r]])))
        (str/split-lines (slurp (if (string? in) (java.io.StringReader. in) in)))
        ))

(defn build-queue [bots]
  ;; For each bot, the code calculates d = manhattan distance to origin and adds (MAX(d-r,0), 1) and (d+r, -1) to a priority queue.
  (into (sorted-map) ;; store dups by adding id to key
        (mapcat (fn [[id [x y z r]]]
                  (let [d (+ (Math/abs ^long x)
                             (Math/abs ^long y)
                             (Math/abs ^long z))]
                    
                    [[[(max 0 (- d r)) id] 1]
                     [[(+ d r) id] -1]])))
        bots))

(defn part2 [in]
  (let [bots (load-bots in)
        q (build-queue bots)

        res (->> (reductions
                  (fn [[c _dist] [[dist id] e]]
                    [(+ c e) dist])
                  [0 nil]
                  q)
                 rest
                 (apply max-key first)
                 second)]
    res))




(comment
  
  (part2 "pos=<10,12,12>, r=2
pos=<12,14,12>, r=2
pos=<16,12,12>, r=4
pos=<14,14,14>, r=6
pos=<50,50,50>, r=200
pos=<10,10,10>, r=5" )
  ;; 12,12,12 -> 36

  (part2 (io/resource "day23/input.txt"))
  ;; => 107279292

  )
