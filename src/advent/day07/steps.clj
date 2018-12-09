(ns advent.day07.steps
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;; I used the following as an example to learn from
;; @see https://github.com/rymndhng/advent-of-clojure/blob/master/src/advent_2018/07.clj

(def input-file "resources/day07/input.txt")

(defn read-input []
  (str/split-lines (slurp input-file)))


(def test-input ["Step C must be finished before step A can begin."
                 "Step C must be finished before step F can begin."
                 "Step A must be finished before step B can begin."
                 "Step A must be finished before step D can begin."
                 "Step B must be finished before step E can begin."
                 "Step D must be finished before step E can begin."
                 "Step F must be finished before step E can begin."])

;; #{["C" "A"] ["B" "E"] ["C" "F"] ["F" "E"] ["A" "D"] ["D" "E"] ["A" "B"]}
(defn build-edges [input]
  (letfn [(tokenize [v] (let [[_ parent _ _ _ _ _ child _ _] (str/split v #" ")]
                          [parent child]
                          ))]
    (into #{} (map tokenize input))))

;; #{"E" "C" "F" "B" "A" "D"}
(defn build-nodes [steps]
  (set (concat (map first steps) (map second steps))))

;; {"A" ("C"), "E" ("D" "F" "B"), "F" ("C"), "D" ("A"), "B" ("A")}
(defn build-dependencies [edges]
  (reduce (fn [acc [k v]] (update acc v conj k)) {} edges))

(defn walk [nodes deps-map]
  (loop [path []
         visited #{}
         unvisited (set nodes)]
    (if (empty? unvisited)
      path
      (let [x (->> unvisited
                   (sort-by (fn [v] [(count (set/difference (set (get deps-map v)) visited))
                                     v]))
                   first)]
        (recur (conj path x)
               (conj visited x)
               (disj unvisited x))))))

(defn part1 []
  (let [edges (build-edges (read-input))
        nodes (build-nodes edges)
        deps-map (build-dependencies edges)]
    ;; (println edges)
    ;; (println nodes)
    ;; (println deps-map)
    (apply str (walk nodes deps-map))))

(comment
  (part1)   ;; "PFKQWJSVUXEMNIHGTYDOZACRLB"
)



