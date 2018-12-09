(ns advent.day07.steps0)

;; I used the following as an example to learn from
;; @see https://raw.githubusercontent.com/Sorceror/aoc2018/master/src/aoc2018/day07.clj

(def input-file "resources/day07/input.txt")

(defn read-input []
  (clojure.string/split-lines (slurp input-file)))


(def test-input ["Step C must be finished before step A can begin."
                 "Step C must be finished before step F can begin."
                 "Step A must be finished before step B can begin."
                 "Step A must be finished before step D can begin."
                 "Step B must be finished before step E can begin."
                 "Step D must be finished before step E can begin."
                 "Step F must be finished before step E can begin."])

;; #{["C" "A"] ["B" "E"] ["C" "F"] ["F" "E"] ["A" "D"] ["D" "E"] ["A" "B"]}
(defn build-edges [input]
  (letfn [(tokenize [v] (let [[_ parent _ _ _ _ _ child _ _] (clojure.string/split v #" ")]
                          [parent child]
                          ))]
    (into #{} (map tokenize input))))

;; #{"C"}
(defn get-roots [edges]
  (let [start-edges (into #{} (map first edges))
        end-edges (into #{} (map second edges))]
    (into (sorted-set) (clojure.set/difference start-edges end-edges))))

;; (find-children "C" (build-edges test-input))
;; #{"A" "F"}
(defn find-children [v edges]
  ;; (into (sorted-set) (map second (filter (fn [[sv _]] (= sv v)) edges)))
  (->> edges
       (filter (fn [[sv _]] (= sv v)))
       (map second)
       (into (sorted-set))))

(defn find-available-for-children [vertices edges]
  (->> vertices
       (map #(find-children % edges))
       (reduce clojure.set/union)
                                        ; input three has multiple roots, even though example does not show it or mention it :|
       (clojure.set/union (get-roots edges))
       (#(clojure.set/difference % vertices))))

;; (find-parents "E" (build-edges test-input))
;; #{"B" "D" "F"}
(defn find-parents [v edges]
  (->> edges
       (filter (fn [[_ sv]] (= sv v)))
       (map first)
       (into #{})))

(defn find-reachable-for-children [seen edges]
  (->> edges
       (find-available-for-children seen)
       (filter #(clojure.set/subset? (find-parents % edges) seen))
       (into (sorted-set))))

(defn find-steps [input]
  (let [edges (build-edges input)
        root (first (get-roots edges))]
    (loop [seen #{root}
           path (vector root)]
      (let [reachable (find-reachable-for-children seen edges)]
        (if (empty? reachable)
            path
            (recur (conj seen (first reachable)) (conj path (first reachable))))))))


(defn part1 []
  (apply str (find-steps (read-input))))

(comment
  (part1)    ;; "PFKQWJSVUXEMNIHGTYDOZACRLB"
)

