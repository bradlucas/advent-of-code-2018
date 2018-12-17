(ns advent.day12.plants
  (:require [clojure.string :as str]))


(def input "resources/day12/input.txt")

(defn read-input []
  (let [lines (str/split-lines (slurp input))
        initial (second (str/split (first lines) #": "))
        rules (vec (rest (rest lines)))]
    {:initial initial
     :rules rules}))

(defn test-input []
  {:initial "#..#.#..##......###...###"
   :rules ["...## => #"
           "..#.. => #"
           ".#... => #"
           ".#.#. => #"
           ".#.## => #"
           ".##.. => #"
           ".#### => #"
           "#.#.# => #"
           "#.### => #"
           "##.#. => #"
           "##.## => #"
           "###.. => #"
           "###.# => #"
           "####. => #"]})

(defn build-array 
  "Convert a string of . and # characters to an array of 0 and 1s
  (build-array \".#\")
  [0 1]
  "
  [str]
  (mapv (fn [x] (if (= \. x) 0 1)) str))

(defn build-state 
  "Build state data structure from initial string.
  Prepend three blanks -1 and -2 so you can match a rule for the first post at 0.
  " 
  [initial]
  (let [initial (apply str (concat "....." initial "....................."))]
    (build-array initial)))

(defn display-state
  "Convert 0 and 1 array back to . and # characters"
  [v]
  (apply str (mapv (fn [c] (if (= c 0) "." "#")) v)))

(defn build-rules
  "Convert list of rule string to rule arrays."
  [rules]
  (letfn [(split-rule [s] (map build-array (clojure.string/split s #" => ")))]
    (map split-rule rules)))

(defn load-input 
  "Call the data-fnc to read initial state string and rules string. Then convert to state and rule arrays."
  [data-fnc]
  (let [{:keys [initial rules]} (data-fnc)
        state (build-state initial)
        rules (build-rules rules)]
    {:state state
     :rules rules}))

(defn apply-rule-position
  "For a given position (idx) in state run the given rule"
  [state rule idx]
  (let [sub (subvec state (- idx 2) (+ idx 3))
        [pattern outcome] rule
        match (= sub pattern)
        val (first outcome)]
    ;; (println "idx: " idx)
    ;; (println "sub: " sub)
    ;; (println "pat: " pattern)
    ;; (println "mat: " match)
    ;; (println "out: " val)
    (if match
      (if (not (zero? val))
        ;; (assoc state idx val)
        ;; (assoc rtn idx val)
        idx))))

(defn apply-rule [state rule]
  ;; update state per rule for each potential position
  ;; for each position starting at 0 which now the third position of state
  ;; get the 5 positions (two before and two after)
  ;; apply the rule
  (let [last (- (count state) 3)]
    ;; (println last)
    (loop [idx 2   ;; -2 -1 0
           ;; rtn (vec (take (count s) (repeat 0)))
           positions []
           ]
      ;; (println idx)
      ;; (println rtn)
      (if (> idx last)
        positions
        (recur (inc idx) (conj positions (apply-rule-position state rule idx)))))))

(defn build-new-state [state idxs]
  (let [new-state (vec (take (count state) (repeat 0)))]
    (reduce #(assoc %1 %2 1) new-state idxs)))

(defn apply-rules [state rules]
  ;; apply rules to state and return
  (loop [rules rules
         positions []]
    (if (empty? rules)
      (build-new-state state (vec (sort (flatten positions))))
      (recur (next rules) (conj positions (remove nil? (apply-rule state (first rules))))))))

(defn sum
  "Input is
[0 0 0 1 0 0 0 0 1 1 0 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1 1 1 0 0 0 0 1 0 1 0 0 1 1 0 0 0 0 0 0 0]
Inital offset is -5
Add the positional values where 1 is placed"
  [v]
  (let [idx-val (map vector (range -5 (inc (count v))) v)]  ;; ([-5 0] [-4 0] [-3 0] [-2 1] [-1 0] [0 0] [1 0] [2 0] [3 1] ...
    (reduce + (remove nil? (map (fn [[idx v]] (if (= 1 v) idx)) idx-val)))))


(defn process [input-fnc cnt]
  (let [{:keys [state rules]} (load-input input-fnc)]    ;; test-input or read-input
    ;; (println state)
    (loop [state state        
           cnt cnt]
      ;; (println (display-state state))
      (if (zero? cnt)
        state
        (recur (apply-rules state rules) (dec cnt))))))


(defn part1-test []
  (sum (process test-input 20)))        ;; 325

(defn part1 []
  (sum (process read-input 20)))        ;; 1987

(comment
  (part1-test)
)

(comment
  (part1)
)


;; ----------------------------------------------------------------------------------------------------
;; Testing


(def input-data (load-input test-input))
(def s (:state input-data))
(def rules (:rules input-data))
(def r (first rules))
