(ns advent.day03.overlap
  (:require [clojure.string :refer [split-lines split]]))


(def input-file "resources/day03/input.txt")

(defn read-input []
  (split-lines (slurp input-file)))

(def v "#1 @ 896,863: 29x19")

;; :id
;; :row
;; :col
;; :width
;; :height
(defn process-line [l]
  (let [[a b c d] (split l #" ")
        id (Integer. (subs a 1))
        [row col] (map #(Integer. %) (split c #",|:"))
        [height width] (map #(Integer. %) (split d #"x"))]
    {:id id 
     :row row
     :col col
     :height height
     :width width}))

;; v
;; {:id 1, :row 896, :col 863, :height 29, :width 19}

;; convert a `claim` into a set of x,y coordinates
;; define a map which represents the fabric which is at least 1000 inches on each side
;; the map will track for each cell the number of claims that cover it

;; to answer the first part's question the map will have to be searched for the number of cells
;; with more than 1 claimant

(defn inc-at [a [row col]]
  (aset a row col (inc (aget a row col)))
  a)

(defn build-fabric [size]
  (to-array-2d (repeat size (repeat size 0))))

(defn build-cells [rows cols]
  (let [s #{}]
    (into s (for [r rows
                  c cols]
              [r c]))))

(defn count-overlays [fabric]
  (let [size (count fabric)]
    ;; count the number of cells with a value > 1
    (count (filter #(> % 1) (map (fn [[row col]] (aget fabric row col)) (build-cells (range size) (range size)))))))

(defn process-claim [a claim]
  (let [{:keys [id row col height width]} claim
        row-range (range row (+ row height))
        col-range (range col (+ col width))]
    ;; (println id row col height width)
    ;; (println row-range)
    ;; (println col-range)
    ;; all the cell positions
    (let [cells (build-cells row-range col-range)]
      (loop [fabric a
             positions cells]
        (if (empty? positions)
          fabric
          (recur (inc-at fabric (first positions)) (rest positions)))))))

(defn part1 []
  (loop [fabric (build-fabric 1000)
        claims (map process-line (read-input))]
    (if (empty? claims)
      (count-overlays fabric)
      (recur (process-claim fabric (first claims)) (rest claims)))))


(comment
  (part1)  ;; 112418
  )


