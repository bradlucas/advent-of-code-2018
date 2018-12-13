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


;; ----------------------------------------------------------------------------------------------------
;; Part2

;; Vary sizes of squares from 1x1 to 300x0
;; Find the square of any size with the largest total power


(defn square-power-level-size [serial-number size x y]
  (let [power (reduce + (map (fn [x] (power-level serial-number (first x) (second x))) (mapcat (fn [x] (map (fn [y] [x y]) (range y (+ y size)))) (range x (+ x size)))))]
    ;; {[x y]
    ;;  power}
    {:point [x y]
     :power power
     :size size
     }
))

(defn solve-size [size]
  (let [serial-number input
        upper-limit (- (+ 300 1) (- size 1))
        points (mapcat (fn [x] (map (fn [y] [x y]) (range 1 upper-limit))) (range 1 upper-limit))]
    ;; (into (sorted-map) (pmap (fn [x] (square-power-level-size serial-number size (first x) (second x))) points))
    (pmap (fn [x] (square-power-level-size serial-number size (first x) (second x))) points)
    ))

(defn solve-part2 []
  (loop [sizes (range 1 20)  ;; 301   @see https://github.com/baritonehands/advent-of-code-2018/blob/master/src/aoc/dec11.clj
         acc []]
    (if (empty? sizes)
      acc
      (recur (next sizes) (conj acc (apply max-key :power (solve-size (first sizes))))))))

(defn part2 []
  (apply max-key :power (solve-part2)))

(comment
  (part2) ;; {:point [234 197], :power 98, :size 14}  => 234,197,14
)

;; range 1 20
;; advent.day11.fuelcell> (time (part2))
;; "Elapsed time: 88461.005642 msecs"
;; {:point [234 197], :power 98, :size 14}
