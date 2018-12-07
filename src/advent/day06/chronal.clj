(ns advent.day06.chronal
  (:require [clojure.string :refer [split-lines split]]))


;; I used the following example to help learn how to solve Day 6
;; @see https://github.com/Sorceror/aoc2018/blob/master/src/aoc2018/day06.clj

(def input-file "resources/day06/input.txt")

(defn read-input []
  (split-lines (slurp input-file)))

(defn test-input [] ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])

;; ([1 1] [1 6] [8 3] [3 4] [5 5] [8 9])
(defn input-to-pos-seq [data]
  ;; (map #(into [] %) (map #(map read-string %) (map #(split % #", ") data))))
  (->> data
       (map #(split % #", "))
       (map #(map read-string %))
       (map #(into [] %))))

;; {0 [1 1], 1 [1 6], 2 [8 3], 3 [3 4], 4 [5 5], 5 [8 9]}
(defn build-id-map-from-pos-seq [pos-seq]
  ;; map-indexed
  ;; Returns a lazy sequence consisting of the result of applying f to 0
  ;; and the first item of coll, followed by applying f to 1 and the second
  ;; item in coll, etc, until coll is exhausted.
  ;; (into (hash-map) (map-indexed (fn [idx itm] [idx itm]) (input-to-pos-seq (test-input))))
  ;;
  ;; {0 "1, 1", 1 "1, 6", 2 "8, 3", 3 "3, 4", 4 "5, 5", 5 "8, 9"}
  (->> pos-seq
       (map-indexed (fn [idx itm] [idx itm]))
       (into (hash-map))))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x2 x1)) (Math/abs (- y2 y1))))

(defn calc-closest-id [p id-map]
  ;; @see https://github.com/Sorceror/aoc2018/blob/master/src/aoc2018/day06.clj
  (let [distances (map (fn [[k k-pos]] [k (manhattan-distance p k-pos)]) id-map)
        min-val (apply min (map second distances))
        min-ids (filter #(= min-val (second %)) distances)]
    (if (= 1 (count min-ids))
      (first (first min-ids))
      \.)))

(defn calc-bounding-box [pos-seq]
  ;; @see https://github.com/Sorceror/aoc2018/blob/master/src/aoc2018/day06.clj
  ;; Find the corner points
  (reduce (fn [[[min-x min-y] [max-x max-y]] [p-x p-y]]
            [[(min min-x p-x) (min min-y p-y)] [(max max-x p-x) (max max-y p-y)]])
          [[(Integer/MAX_VALUE) (Integer/MAX_VALUE)] [0 0]] pos-seq))

(defn calc-board-ids [pos-seq]
  ;; @see https://github.com/Sorceror/aoc2018/blob/master/src/aoc2018/day06.clj
  (let [[[min-x min-y] [max-x max-y]] (calc-bounding-box pos-seq)
        points (for [y (range min-y (inc max-y)) x (range min-x (inc max-x))] [x y])
        id-map (build-id-map-from-pos-seq pos-seq)]
    (map #(calc-closest-id %1 id-map) points)))

(defn biggest-non-infinite-area [pos-seq]
  ;; @see https://github.com/Sorceror/aoc2018/blob/master/src/aoc2018/day06.clj
  (let [board (calc-board-ids pos-seq)
        [[min-x _] [max-x _]] (calc-bounding-box pos-seq)
        bbox-w (inc (- max-x min-x))
        top (take bbox-w board)
        bottom (take-last bbox-w board)
        left-side (take-nth bbox-w board)
        right-side (reverse (take-nth bbox-w (reverse board)))
        border-ids (filter #(not= % \.) (distinct (concat top bottom left-side right-side)))
        filtered-board (replace (zipmap border-ids (repeat \∞)) board)
        counts (frequencies filtered-board)
        areas (dissoc counts \. \∞)]
    (apply max-key second areas)))


;; test-input
;; Id:   17
;; Size: 4


;; read-input
;; Id:   4171
;; Size: 23

(defn part1 []
  (let [[id size] (biggest-non-infinite-area (input-to-pos-seq (read-input)))]
    (println (str "Id:   " id))
    (println (str "Size: " size))
    ))

(comment
  (part1)    ;; 4171
)


;; Part 2
;; What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?

(defn calc-all-distances [p id-map]
  (reduce + (map (fn [[_ k-pos]] (manhattan-distance p k-pos)) id-map)))

(defn calc-board-distances [dist pos-seq]
  ;; @see https://github.com/Sorceror/aoc2018/blob/master/src/aoc2018/day06.clj
  (let [[[min-x min-y] [max-x max-y]] (calc-bounding-box pos-seq)
        points (for [y (range min-y (inc max-y)) x (range min-x (inc max-x))] [x y])
        id-map (build-id-map-from-pos-seq pos-seq)
        board-dist (map #(calc-all-distances %1 id-map) points)
        filtered-board (map #(if (< %1 dist) %1 \.) board-dist)]
    filtered-board))

(defn calc-area-size-with-dist-to-all-threshold [dist pos-seq]
  (count (filter #(not= \. %) (calc-board-distances dist pos-seq))))

(defn part2  []
  (let [dist 10000
        pos-seq (input-to-pos-seq (read-input))]
    (calc-area-size-with-dist-to-all-threshold dist pos-seq)))

(comment
  (part2)   ;; 39545
)
