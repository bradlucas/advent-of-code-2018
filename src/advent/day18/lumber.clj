(ns advent.day18.lumber
  (:require [clojure.string :as str]))


(def input-file "resources/day18/input.txt")


(defn load-input []
  (str/split-lines (slurp input-file)))
