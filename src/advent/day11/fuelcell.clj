(ns advent.day11.fuelcell)


(def input 7672)

(def grid [300 300])


;; coordinate ranges from 1 to 300
;; x,y
;; 1,1 .................   300,1
;;
;; 1,300 ...............   300, 300

;; 3x3 square == fuel cell


;; power level of a given fule cell
;;
;; rack-id = X plus 10
;; power-level = rack-id * Y
;; power-level = power-level + grid-serial-number  (ie, the puzzle input)
;; power-level = power-level * rack-id
;; keep the 100's digit
;; substract 5

(defn extract-hundreds [n]
  (if (< n 100)
    0
    (Integer/parseInt (str (nth (reverse (str n)) 2)))))

(defn power-level [serial-number x y]
  (let [rack-id (+ x 10)]
    (-> rack-id
         (* y)
         (+ serial-number)
         (* rack-id)
         (extract-hundreds)
         (- 5))))

(defn square-power-level [serial-number x y]
  (let [power (reduce + (map (fn [x] (power-level serial-number (first x) (second x))) (mapcat (fn [x] (map (fn [y] [x y]) (range y (+ y 3)))) (range x (+ x 3)))))]
    {[x y]
     power}))


(defn solve []
  (let [serial-number input
        points (mapcat (fn [x] (map (fn [y] [x y]) (range 1 (- 300 1)))) (range 1 (- 300 1)))]
    ;; (apply max (map (fn [x] (square-power-level serial-number (first x) (second x))) points))
    (into (sorted-map) (map (fn [x] (square-power-level serial-number (first x) (second x))) points))
    )
)

(defn part1 []
  (apply max-key val (solve)))


(comment
  (part1)    ;; [[22 18] 29]
)

