(ns advent.core
  (:require [advent.day01.calibration :as day01])
  (:gen-class))

(defn -main
  [& args]
  (do
    (println "Advent of Code 2018")
    (println (day01/run-part1))
    (println (day01/run-part2))))
