(ns advent.day13.carts
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; --------------------------------------------------------------------------------------------------------------
;; The code below was developed after an initial attempt without success then by reviewing the following example.
;;
;; https://github.com/baritonehands/advent-of-code-2018/blob/master/src/aoc/dec13.clj
;;
;; If curious, go see baritonehands' example for an original solution.
;; Thanks to him for providing a solution to learn from.

;; Other examples that were reviewed:
;; https://github.com/athos/advent-of-code-2018/blob/master/src/advent_of_code_2018/day13.clj
;; https://github.com/dandorman/advent-of-code-2018/blob/master/src/aoc2018/d13.cljc


(def input "resources/day13/input.txt")

(defn read-input [] (str/split-lines (slurp input)))

(defn test-input [] (str/split-lines (slurp "resources/day13/test.txt")))

(def cart-characters #{\^ \v \> \<})               ;; carts are one of these (^, v, >, <)

(def cart-underneath {\^ \| \v \| \> \- \< \-})    ;; underneath a cart is either a | or a -

(defn load-map 
  "Call the data-fnc and build the world-map. Include only points with values (ignore spaces).
  Key == [col row]
  Value is the character c and if c is a cart then it is vector [c 0 character-underneath]"
  [data-fnc]
  (let [lines (data-fnc)
        acc {}   ;; (sorted-map)
        ]
    (loop [lines lines
           row 0
           acc acc]
      (if (empty? lines)
        acc
        (recur (rest lines)
               (inc row)
               (let [line (first lines)]
                 (loop [line line
                        col 0
                        acc acc]
                   (if (empty? line)
                     acc
                     (recur (rest line) (inc col) (let [c (first line)]
                                                    (if (not= c \space)                           ;; ignore space
                                                      ;; (assoc acc [row col] (first line))
                                                      (assoc acc [col row] (if (cart-characters c)
                                                                             [c 0 (cart-underneath c)]
                                                                             c))
                                                      acc)))))))))))

(defn find-carts 
  "Find carts and sort them by their row position.
  Cars are defined by the values which are vectors.
  The map is defined as [col row] [v 0 v]"
  [m]
  (let [l (filter #(vector? (val %)) m)]
    ;; (sort-by (juxt (comp second first) ffirst) )
    
    ;; sort-by (second (first %)) and where this has equals then continue and search by ffirst
    ;; In other words, sort by row and then col
    (sort-by (juxt (comp second first) ffirst) l)))

(defn move-next
  ""
  [[col row] direction]
  (case direction
    \> [(inc col) row]
    \< [(dec col) row]
    \^ [col (dec row)]
    \v [col (inc row)]))


(def left {\> \^ 
           \< \v 
           \^ \<
           \v \>})

(def right {\> \v
            \< \^
            \^ \>
            \v \<})

(def turn-idx [left identity right])

(defn intersection [[direction idx]]
  (let [next-direction ((turn-idx idx) direction)]
    [next-direction (mod (inc idx) 3) \+]))

(def turn-mappings
  {[\/ \>] \^
   [\\ \>] \v
   [\/ \<] \v
   [\\ \<] \^
   [\/ \^] \>
   [\\ \^] \<
   [\/ \v] \<
   [\\ \v] \>})

(defn turn-cart 
  "m is the map of the tracks
  [col row] is the current position
  cart is [direction index under-char"
  [m [col row] [direction idx :as cart]]
  (let [track (m (move-next [col row] direction))]
    (cond
     (vector? track) (last track)                                           ;; cart
     (= track \+) (intersection cart)                                       ;; crossroads
     :else [(get turn-mappings [track direction] direction) idx track])))   ;; TODO turn-mappings to function

(defn collisions [m]
  (:collisions (meta m)))

(defn collide [m next-pos]
  ;; does moving the cart cause a collision
  (if (vector? (m next-pos))
    true))

(defn get-under [m pos]
  ;; return the under value of the cart at po
  (let [[_ _ under] (get m pos)]
    under))

(defn move-cart [m pos [direction _ under :as cart]]
  (if (not (vector? (m pos)))
    m
  (let [next-pos (move-next pos direction)]
    (if (collide m next-pos)
      ;; erase the current cart and the cart at next-pos
      (-> (vary-meta m update :collisions conj next-pos)
          (assoc next-pos (get-under m next-pos))
          (assoc pos under)
          )
      (-> m
          (assoc next-pos (turn-cart m pos cart))   ;; move cart to next position
          (assoc pos under)    ;; cart moved, move under value into it's place
          )))))

(defn move-cart-debug [m pos cart]
  (let [before (count (find-carts m))
        rtn (move-cart m pos cart)
        after (count (find-carts rtn))]
    ;; (if (not= before after)
    ;;   (println (str "move-cart-debug " before " " after)))
    rtn))

(defn move-carts [m]
  (reduce (fn [m [pos cart]] (move-cart m pos cart)) m (find-carts m)))


;; ----------------------------------------------------------------------------------------------------
;; Debugging

(def m (load-map test-input))
(def pp clojure.pprint/pprint)

(defn world-size [m]
  (let [width (apply max (map (fn [[[_ row] _]] row) m))
        height (apply max (map (fn [[[col _] _]] col) m))]
    {:width width
     :height height}))

(defn print-world [m]
  (let [{:keys [width height]} (world-size m)]
    ;; (println "width:" width)
    ;; (println "height: " height)
    ;; (println "\n")
    (doseq [row (range 0 (inc width))
          col (range 0 (inc height))]
      (do
        ;; (println "[col, row] [" col "," row "]" )
        (let [v (get m [col row] \space)
              c (if (vector? v) (first v) v)]
          (print c))
        (if (= height col)
          (print "\n"))))
    m))

;; ----------------------------------------------------------------------------------------------------
;; Part 1

(defn part1 [data-fnc]
  (let [m (load-map data-fnc)]
    (->> (iterate move-carts m)
         (drop-while (comp empty? collisions))
         (first)
         (collisions)
         (time))))

(comment
  (part1 read-input)
  ;; "Elapsed time: 489.14499 msecs"
  ;; ([136 36])
)

;; ----------------------------------------------------------------------------------------------------
;; Part 2

(defn part2 [data-fnc]
  (let [m (load-map data-fnc)]
    (->> (iterate move-carts m)
         (drop-while #(not= 1 (count (find-carts %))))
         (first)
         (find-carts)
         (time))))

(comment
  (part2 read-input)
  ;; "Elapsed time: 77974.914995 msecs"
  ;; ([[53 111] [\^ 2 \|]])
)
