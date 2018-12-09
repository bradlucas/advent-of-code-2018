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


;; ----------------------------------------------------------------------------------------------------
;; Part 2

;; Each step is 60 seconds plus an amount corresponding to its letter: A=1, ... Z=26
;;
;; 5 works with 60 secon step durations. How long to complete all the steps


(defn initialize [deps-map steps step-time num-workers]
  {:seconds -1
   :workers []
   :done []
   :queue (set (concat
                (map first steps)
                (map second steps)))
   :deps-map deps-map
   :step-time step-time
   :num-workers num-workers
   })

(defn work [{:keys [workers] :as system}]
  (-> system
      (update :seconds inc)
      (update :workers (fn [workers]
                         (map #(update % 1 dec) workers)))))

(defn job-time [job step-time]
  ;; job is "C"
  (let [letter (.charAt job 0)]
    ;; Character/getNumericValue("A") returns 10 to subtract 9
    (+ step-time (- (Character/getNumericValue letter) 9))))

(defn next-job [{:keys [deps-map queue done]}]
  (->> queue
       (filter #(zero? (count (clojure.set/difference (set (get deps-map %))
                                                      (set done)))))
       (sort-by first)                  ;keep the next job algorithm as above
       first))

(defn schedule [{:keys [workers queue step-time num-workers] :as system}]
  (if (or (<= num-workers (count workers))
          (empty? queue))
    system
    (if-let [job (next-job system)]
      (recur (-> system
                 (update :workers conj [job (job-time job step-time)])
                 (update :queue disj job)))
      system)))

(defn update-completed-jobs [{:keys [workers] :as system}]
  (let [done-workers (group-by #(zero? (second %)) workers)]
    (-> system
        (update :done into (map first (get done-workers true)))
        (assoc :workers (get done-workers false)))))

(defn tick [{:keys [workers queue] :as system}]
  (if (and (empty? workers)
           (empty? queue))
    system
    (-> system
        (update-completed-jobs)
        (schedule)
        (work))))

(defn forever [v]
  (cons v (lazy-seq (forever (tick v)))))

(defn part2 []
  (let [edges (build-edges (read-input))
        dependencies (build-dependencies edges)
        node (build-nodes edges)
        step-time 60
        num-workers 5]
    (:seconds (nth (rest (forever (initialize dependencies edges step-time num-workers))) 1500))))

(comment
  (part2)  ;; 864
)
