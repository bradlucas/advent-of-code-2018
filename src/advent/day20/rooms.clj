(ns advent.day20.rooms
  (:require [clojure.string :as str]))

;; ----------------------------------------------------------------------------------------------------------
;; learning from 
;; @see https://github.com/Average-user/adventofcode-clj-2018/blob/master/src/adventofcode_clj_2018/day20.clj


(def input-file "resources/day20/input.txt")


(defn load-input []
  ;; remove starting ^ and ending $ characters
  ;; turn input string into a series of characters
  (->> (slurp input-file)
       (filter #{\( \) \| \N \W \S \E})))

(defn move [dir [x y]]
  (case dir
    \N [x (dec y)]
    \E [(inc x) y]
    \S [x (inc y)]
    \W [(dec x) y]))

(defn build-graph' [stack ns es pos input]
  (if (empty? input)
    [(map second ns) es]
    (case (first input)
      \( (recur (cons pos stack) ns es pos           (rest input))
      \| (recur stack            ns es (first stack) (rest input))
      \) (recur (rest stack)     ns es (first stack) (rest input))
      (let [l     (count ns)
            pos'  (move (first input) pos)
            v'    (get ns pos')
            ns'   (if (nil? v') (assoc ns pos' l) ns)
            v     (if (nil? v') l v')]
        (recur stack ns' (conj es [(ns pos) v]) pos' (rest input)))))) 

(defn build-graph [input]
  (let [[nodes edges] (build-graph' '() {[0 0] 0} #{} [0 0] input)]
    (into {} (map (fn [n] [n (keep (fn [[a b]] (when (= n a) b)) edges)]) nodes))))

(defn nodes-depth [node neighbors]
  (letfn [(options [seen n] (remove seen (neighbors n)))]
    (loop [front (options #{node} node)
           seen #{node}
           ds [0]
           depth 1]
      (if (empty? front)
        ds
        (recur (mapcat (partial options seen) front)
               (reduce conj seen front)
               (reduce conj ds (map (constantly depth) front))
               (inc depth))))))


;; Testing

(def test-input "^ENWWW(NEEE|SSE(EE|N))$")
(def test-input2 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$\n")
(def test-input3 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")

(defn load-string [s]
  (->> s
       (filter #{\( \) \| \N \W \S \E})))


;;  ----------------------------------------------------------------------------------------------------
;; Part 1

(defn part1 []
  (let [input (load-input)]
    (->> input
         build-graph
         (nodes-depth 0)
         (reduce max))))


(comment
  (part1)     ;; 3314
)


;;  ----------------------------------------------------------------------------------------------------
;; Part 2

(defn part2 []
  (let [input (load-input)]
    (->> input
         build-graph
         (nodes-depth 0)
         (filter (partial <= 1000))
         count)))


(comment
  (part2)      ;; 8550
)


