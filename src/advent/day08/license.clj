(ns advent.day08.license
  (:require [clojure.string :as str]))

;; The following was used as and example to solve this problem
;; @see https://github.com/dandorman/advent-of-code-2018/blob/master/src/aoc2018/d08.cljc

(def input-file "resources/day08/input.txt")

(defn tokenize [l]
  (mapv #(Integer. %) l))

(defn read-input []
  (-> input-file
      slurp
      str/trim
      (str/split #" ")
      tokenize))

(defn test-input []
  (let [s "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"]
    (-> s
        (str/split #" ")
        tokenize)))

;;
;; "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
;;
;; A (2 3)
;;   B (0 3)
;;     [10 11 12]
;;   C (1 1)
;;     D (0 1)
;;       [99]
;;     [2]
;;   [1 1 2]
;;


(defn build-tree [numbers]
  (let [[num-child num-meta & numbers] numbers
        [numbers children] (reduce (fn [[nums children] _]
                                     (let [[nums child] (build-tree nums)]
                                       [nums (conj children child)]))
                                   [numbers []]
                                   (range num-child))
        [meta numbers] (split-at num-meta numbers)]     ;; chop off the list of meta numbers
    [numbers {:meta meta :children children}]))
  

(defn part1 []
  (let [tree (-> (read-input) build-tree peek)]
    (reduce + 
        ;; mapcat to build single list    
        (mapcat :meta 
                ;; @see https://clojuredocs.org/clojure.core/tree-seq
                ;; build a tree via depth-first walk
                ;; the child nodes are selected with :child
                ;; and must contain elements
                (tree-seq #(seq (:children %)) :children tree))
        )
    ))


(comment
  (part1)    ;; 41926
)



;; Part 2

;; (-> (test-input) build-tree peek)

;;
;; "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
;;
;; A (2 3)
;;   B (0 3)
;;     [10 11 12]
;;   C (1 1)
;;     D (0 1)
;;       [99]
;;     [2]
;;   [1 1 2]
;;

;; {:meta (1 1 2), :children [{:meta (10 11 12), :children []} {:meta (2), :children [{:meta (99), :children []}]}]}

;; If no children then the node's value is the sum of it's metadata
;; If has children then the metadata entries are indexes to the child nodes if presnt

;; A -> [1 1 2] -> [1 2] => means value of B, B and C

;; B has no children so sum of [10 11 12] => 33

;; C has children so metadata [2] refers to the second node which doesn't exist => 0

;; So, A's [1 1 2] -> [33 33 0] == 66

(defn walk [tree]
  ;; (println tree)
  (if (map? tree)
    (let [{:keys [meta children]} tree]
      ;; has children
      (if (seq children)
        ;; children is a list, index into it and get the value of each child
        (assoc tree :value (reduce (fn [sum idx]
                                     (let [child (get children idx)]
                                       (+ sum (get child :value 0))))
                                   0
                                   (map dec meta)))
        ;; else, just add the meta values
        (assoc tree :value (reduce + meta))))
    tree)
  )

;; (def tree (-> (read-input) build-tree peek))
;; (def tree (-> (test-input) build-tree peek))


(defn part2 []
  (let [tree (-> (read-input) build-tree peek)]
    (:value (clojure.walk/postwalk walk tree))))


(comment
  (part2)   ;; 24262
)
