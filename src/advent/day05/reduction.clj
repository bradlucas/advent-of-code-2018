(ns advent.day05.reduction)

(def input-file "resources/day05/input.txt")

(defn read-file []
  (clojure.string/trim (slurp input-file)))


;; drop characters where the lowercase and uppercase of the same letter are paired together
;; ie, drop `aA` or `cC` etc
;;
;; 

(def test-string "dabAcCaCBAcCcaDA")


(defn pair? [[a b]]
  ;; return true if a and b are the same character but differ in case
  (= 32 (Math/abs (- (int a) (int b)))))

(defn partition-data [data]
  (if (odd? (count data))
    (map vec (partition 2 data \space))
    (map vec (partition 2 data))))

(defn filter-pairs [s]
  (let [data (vec s)
        odd (odd? (count data))
        last-char (if odd (last data))]
    ;; if an odd length string, save the last character and add it on the way out
    (let [return (apply str (flatten (filter #(not (pair? %)) (partition 2 s))))]
      (if odd
        (apply str (concat return (str last-char)))
        return))))

(defn process-data [data]
  (let [data data]
    (let [d (filter-pairs data)
          first-char (subs d 0 1)
          rest-string (filter-pairs (subs d 1))]
      (apply str (concat first-char rest-string)))))


(defn process-part1 [s]
    (loop [data (process-data s)
           last-count 0]
      (let [cnt (count data)]
        ;; (println last-count cnt)
        (if (= cnt last-count)
          cnt
          (recur (process-data data) cnt)))))

(defn part1 []
  (process-part1 (read-file))
)

(comment
  (let [data (vec (read-file))]
    (loop [data (process-data data)
           last-count 0]
      (let [cnt (count data)]
        ;; (println last-count cnt)
        (if (= cnt last-count)
          cnt
          (recur (process-data data) cnt))))))

(comment
  (part1)    ;; 11814
)



;; Part 2
;; Which pair is the best to remove then reduce
;;
;; Find all the unique characters in the string
;; For each remove each a/A pair then reduce. Save length
;; Which produced the shortest string

(def alphabet (map char (range (int \a) (int \z))))

(defn char-pair? [c [a b]]
  ;; is [a b] a pair of cC
  (if (pair? [a b])
    (or (= c a) (= c b))))


(defn remove-pairs [s c]
  (loop [s s
         acc []]
    (if (= 0 (count s))
      acc
      (if (= 1 (count s))
        (conj acc s)
        (let [[a b] s]
          (recur (next (next s)) (if (not (char-pair? c [a b])) (conj acc a b) acc)))))))


;; submit lower case
(defn remove-both-chars [s c]
  ;; drop c and C from c
  (let [c c
        C (char (- (int c) 32))]
    (loop [s s
           acc []]
      (if (= 0 (count s))
        (apply str acc)
        (recur (next s) (let [a (first s)]
                          (if (not (or (= a c) (= a C)))
                            (conj acc a)
                            acc)))))))


(defn process-part2 [s]
  (loop [alphabet alphabet
         acc {}
         ]
    (if (empty? alphabet)
      (val (apply min-key val acc))
      (let [c (first alphabet)]
        (recur (next alphabet) (assoc acc c (process-part1 (remove-both-chars s c))))))))


(defn part2-test []
  (let [s "dabAcCaCBAcCcaDA"]
    (process-part2 s)))


(defn part2 []
  (process-part2 (read-file)))


(comment
  (part2)   ;; 4282
)
