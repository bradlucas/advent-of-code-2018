>(ns advent.day10.stars)


(def input-file "resources/day10/input.txt")

(defn read-input []
  (clojure.string/split-lines (clojure.string/trim (slurp input-file))))


(defn test-input []
  ["position=< 9,  1> velocity=< 0,  2>"
   "position=< 7,  0> velocity=<-1,  0>"
   "position=< 3, -2> velocity=<-1,  1>"
   "position=< 6, 10> velocity=<-2, -1>"
   "position=< 2, -4> velocity=< 2,  2>"
   "position=<-6, 10> velocity=< 2, -2>"
   "position=< 1,  8> velocity=< 1, -1>"
   "position=< 1,  7> velocity=< 1,  0>"
   "position=<-3, 11> velocity=< 1, -2>"
   "position=< 7,  6> velocity=<-1, -1>"
   "position=<-2,  3> velocity=< 1,  0>"
   "position=<-4,  3> velocity=< 2,  0>"
   "position=<10, -3> velocity=<-1,  1>"
   "position=< 5, 11> velocity=< 1, -2>"
   "position=< 4,  7> velocity=< 0, -1>"
   "position=< 8, -2> velocity=< 0,  1>"
   "position=<15,  0> velocity=<-2,  0>"
   "position=< 1,  6> velocity=< 1,  0>"
   "position=< 8,  9> velocity=< 0, -1>"
   "position=< 3,  3> velocity=<-1,  1>"
   "position=< 0,  5> velocity=< 0, -1>"
   "position=<-2,  2> velocity=< 2,  0>"
   "position=< 5, -2> velocity=< 1,  2>"
   "position=< 1,  4> velocity=< 2,  1>"
   "position=<-2,  7> velocity=< 2, -2>"
   "position=< 3,  6> velocity=<-1, -1>"
   "position=< 5,  0> velocity=< 1,  0>"
   "position=<-6,  0> velocity=< 2,  0>"
   "position=< 5,  9> velocity=< 1, -2>"
   "position=<14,  7> velocity=<-2,  0>"
   "position=<-3,  6> velocity=< 2, -1>"])

;; (defn test-input1 []
;;   ["position=< 9,  1> velocity=< 0,  2>"
;;    "position=< 15,  10> velocity=< 0,  2>"
;;    ])


(defn parse [s]
  ;; extract position and velocity
  (let [[_ p v] (re-find #".*<(.*)>.*<(.*)>" s)
        pos (map #(Integer. %) (map clojure.string/trim (clojure.string/split p #",")))
        vel (map #(Integer. %) (map clojure.string/trim (clojure.string/split v #",")))]
    [pos vel]))

(defn load-points [input-fnc]
  (mapv parse input-fnc))

(defn apply-velocity [[pos vel]]
  (let [[px py] pos
        [vx vy] vel]
  [[(+ px vx) (+ py vy)] [vx vy]]))

(defn step [points]
  ;; for each of the points apply the velocity to the current position and update
  (loop [points points
         acc []]
    (if (empty? points)
      acc
      (recur (rest points) (conj acc (apply-velocity (first points)))))))



(defn data-size [points]
  (let [pts (map first points)
        xs (map first pts)
        ys (map second pts)
        x-min (apply min xs)
        x-max (apply max xs)
        y-min (apply min ys)
        y-max (apply max ys)
        size (* (Math/abs (- x-min x-max)) (Math/abs (- y-min y-max)))]
    {:x-min x-min
     :y-min y-min
     :x-max x-max
     :y-max y-max
     :size size}
    )


  ;; (let [x-offset (apply min (map first (map first points)))
  ;;       y-offset (apply min (map second (map first points)))
  ;;       x-max    (apply max (map first (map first points)))
  ;;       y-max    (apply max (map second (map first points)))
  ;;       ]
  ;;   ;; (println "x-offset " x-offset)
  ;;   ;; (println "y-offset " y-offset)
  ;;   ;; (println "x-max" x-max)
  ;;   ;; (println "y-max" y-max)
  ;;   [x-offset
  ;;    y-offset
  ;;    x-max
  ;;    y-max])

)

;; min x is the left col
;; max x is the right col
;; (defn display [[x-offset y-offset x-max y-max] points]
;;   (let [k (into #{} (map vec (map first points)))]
    
;;     (doseq [y (range y-offset (inc y-max))]
;;       (do
;;         (doseq [x (range x-offset (inc x-max))]
;;           (if (contains? k [x y])
;;             (print "#")
;;             (print "."))
;;           )
;;         (println "\n")
;;         )
;;       )
;;     )
;; )

(defn display [points]
  (let [{:keys [x-min y-min x-max y-max]} (data-size points)
        k (into #{} (map vec (map first points)))]
    (doseq [y (range y-min (inc y-max))]
      (do
        (doseq [x (range x-min (inc x-max))]
          (if (contains? k [x y])
            (print "#")
            (print "."))
          )
        (println "\n")
        )
      )
    ))



;; the data needs to shrink
;; loop until the bounding area of the data reverses

;; (defn smaller-box [a b]
;;   (<= (* (first a) (second a)) (* (first b) (second b))))

(defn run [points]
  (loop [points points
         cnt 0]
      (let [points points
            points-size (:size (data-size points))
            next-step (step points)
            next-step-size (:size (data-size next-step))]
        (if (<= next-step-size points-size)
          (recur next-step (inc cnt))
          [points cnt]))))

(defn part1 [points]
  (let [[points cnt] (run points)]
    (println "Program took " cnt " iterations to appear")
    (println "")
    (display points)))


(defn part1-test []
  (part1 (load-points (test-input))))


;; Test Input

;; Program took  3  iterations to appear
;; #...#..###
;; #...#...#.
;; #...#...#.
;; #####...#.
;; #...#...#.
;; #...#...#.
;; #...#...#.
;; #...#..###


(comment
  (time (part1 (load-points (read-input))))
)


;; Program took  10681  iterations to appear

;; .####...######.....###..#....#..#....#...####...#....#..######
;; #....#..#...........#...#...#...#....#..#....#..#....#.......#
;; #.......#...........#...#..#....#....#..#.......#....#.......#
;; #.......#...........#...#.#.....#....#..#.......#....#......#.
;; #.......#####.......#...##......######..#.......######.....#..
;; #..###..#...........#...##......#....#..#..###..#....#....#...
;; #....#..#...........#...#.#.....#....#..#....#..#....#...#....
;; #....#..#.......#...#...#..#....#....#..#....#..#....#..#.....
;; #...##..#.......#...#...#...#...#....#..#...##..#....#..#.....
;; .###.#..######...###....#....#..#....#...###.#..#....#..######

;; "Elapsed time: 4393.010305 msecs"
;; nil

;; GEJKHGHZ



;; Part2
;; Number of seconds/iterations

;; From above

;; 10681

