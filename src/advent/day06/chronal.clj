(ns advent.day06.chronal
  (:require [clojure.string :refer [split-lines]]))


(def input-file "resources/day06/input.txt")

(defn read-input []
  (split-lines (slurp input-file)))
