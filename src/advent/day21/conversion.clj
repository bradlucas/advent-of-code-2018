(ns advent.day21.conversion)


(def input-file "resources/day21/input.txt")


(defn load-input []
  (clojure.string/split-lines (slurp input-file)))


;; Builds on Day 16 and Day 19
