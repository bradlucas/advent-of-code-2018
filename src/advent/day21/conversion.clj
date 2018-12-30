(ns advent.day21.conversion
  (:require [clojure.string :as str]
            [advent.day19.register :as day19]))


(def input-file "resources/day21/input.txt")

(defn load-input []
  (let [[line1 & instructions] (str/split-lines (slurp input-file))
        ip (read-string (last (str/split line1 #" ")))
        ]
    [ip (day19/parse-instructions instructions)]))


;; Builds on Day 16 and Day 19


;; the following doesn't return
;; others suggest reverse-engineering the program
(defn part1 []
  (let [[ipr instructions] (load-input)]
    (first (day19/run ipr instructions [0 0 0 0 0 0]))))
