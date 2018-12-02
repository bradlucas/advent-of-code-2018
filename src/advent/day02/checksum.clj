(ns advent.day02.checksum
  (:require [clojure.string :refer [split-lines]]))


(def input-file "resources/day02/input.txt")

(defn read-input []
  (split-lines (slurp input-file)))

(def v "tqzvwnogbarflkpcbdewsmjhxi")

(defn process-string-to-set [s]
  (set (distinct (filter #(or (= 2 %) (= 3 %)) (vals (frequencies s))))))

(defn update-two-cnt [acc s]
  (if (contains? s 2)
    (update-in acc [:two] inc)
    acc))

(defn update-three-cnt [acc s]
  (if (contains? s 3)
    (update-in acc [:three] inc)
    acc))

(defn process [acc s]
  (update-three-cnt (update-two-cnt acc s) s))

(defn part1 []
  (loop [data (read-input)
         acc {:two 0 :three 0}]
    (if (empty? data)
      (* (:two acc) (:three acc))
      ;;(prn acc)
      (recur (next data) (process acc (process-string-to-set (first data)))))))

(comment
  (part1)    ;; 5750
)


;; find boxew which differ by exactly one character at the same position

;; sort
;; (sort read-input)

(defn differ-by-one-char
  "Return true if a and b differ by one char in the same position
  For example, abc adc"
  [a b]
  (let [len (count a)
        num-matches (count (filter identity (map = a b)))]
    ;; number of matches positions should be one less then than the number of positions
    (= (- len 1) num-matches)))


(defn common-chars
  "Return the chars that positionally match"
  [a b]
  (apply str (map (fn [a b] (if (= a b) a nil)) a b)))

(def test-input
["abcde"
"fghij"
"klmno"
"pqrst"
"fguij"
"axcye"
"wvxyz"]
)

(defn part2 []
  (loop [l (sort (read-input))]
    (let [a (first l)
          b (second l)]
      (if (differ-by-one-char a b)
        (do
          (println a b)
          (common-chars a b))
        (recur (next l))))))


;; return the common letters between the two box ids


(comment
  (part2)   ;; "tzyvunogzariwkpcbdewmjhxi"
)
