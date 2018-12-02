(ns advent.day02.checksum
  (:require [clojure.string :refer [split-lines]]))


(def input-file "resources/day02/input.txt")

(defn read-input []
  (split-lines (slurp input-file)))

(def v "tqzvwnogbarflkpcbdewsmjhxi")

(defn process-string-to-set [s]
  (set (distinct (filter #(or (= 2 %) (= 3 %)) (vals (frequencies s))))))

(defn update-two-cnt [acc s]
  (if (contains? s 2)
    (update-in acc [:two] inc)
    acc))

(defn update-three-cnt [acc s]
  (if (contains? s 3)
    (update-in acc [:three] inc)
    acc))

(defn process [acc s]
  (update-three-cnt (update-two-cnt acc s) s))

(defn run-part1 []
  (loop [data (read-input)
         acc {:two 0 :three 0}]
    (if (empty? data)
      (* (:two acc) (:three acc))
      ;;(prn acc)
      (recur (next data) (process acc (process-string-to-set (first data)))))))

(comment
  (run-part1)    ;; 5750
)


