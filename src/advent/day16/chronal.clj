(ns advent.day16.chronal
  (:require [clojure.string :as str]))


(def pp clojure.pprint/pprint)


(defn parse-state
  "Parse 
Before: [1 2 3 4]
into
  [1 2 3 4]"
  [s]
  (let [[_ v] (str/split s #"\[")
        len (count v)
        l (.substring v 0 (dec len))]
    (mapv #(Integer/parseInt %) (str/split l #", "))))

(defn parse-inst
  "Parse the string with 4 integers into a vector of 4 integer"
  [s]
  (mapv #(Integer/parseInt %) (str/split s #" ")))

(defn parse-before [s]
  (parse-state s))

(defn parse-after [s]
  (parse-state s))

(defn load-samples
  "Read the four line samples
Before: [2, 1, 1, 0]
5 1 0 1
After:  [2, 0, 1, 0]

"
  []
  (let [input-file "resources/day16/input1.txt"
        input-lines (str/split-lines (slurp input-file))]
    ;; (into [] (map (fn [[before inst after _] [before insta after]] (partition 4 input-lines))))
    ;; (partition 4 (str/split-lines (slurp "resources/day16/input1.txt")))

    (map (fn [[b i a _]] [(parse-before b) (parse-inst i) (parse-after a)]) (partition 4 (str/split-lines (slurp "resources/day16/input1.txt"))))))

(defn load-test-program []


)

