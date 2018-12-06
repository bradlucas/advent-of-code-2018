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

(defn part1 []
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
