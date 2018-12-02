(ns advent.day01.calibration
(:require [clojure.string :refer [split-lines]]))


(def input-file "resources/day01/input.txt")

(defn read-input []
  (split-lines (slurp input-file)))


(defn split [v]
  (map #(apply str %) (split-at 1 v)))

(defn add-sub [[oper num] val]
  (let [o oper
        n (Integer. num)]
    (if (= oper "-")
      (- val n)
      (+ val n))))

(defn run-part1 []
  (println "Day 01 - Part 1")
  (let [start 0]
    (loop [data (map split (read-input))
           acc 0]
      (if (empty? data)
        acc
        (recur (rest data) (add-sub (first data) acc))))))

(comment
  (run-part1)   ;; 427
)

(def test-input
  ["+1" "-2" "+3" "+1" "+1" "-2" "+10"])

(defn run-part2 []
  (println "Day 01 - Part 2")

  (let [start 0]
    (loop [data (map split (read-input))
           seen #{}
           acc 0]
      (if (empty? data)
        ;; Note: you may need to run through the list multiple times
        (recur (map split (read-input)) seen acc)
        (let [next-val (add-sub (first data) acc)]
          (if (contains? seen next-val)
              next-val
            (recur (rest data) (conj seen next-val) next-val)))))))

(comment
  (run-part2)  ;; 341
  )

