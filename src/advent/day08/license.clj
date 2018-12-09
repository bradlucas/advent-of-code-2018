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


