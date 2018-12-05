(ns advent.day04.repose
  (:require [clojure.string :refer [split-lines]]))


(def input-file "resources/day04/input.txt")

(defn read-input []
  (split-lines (slurp input-file)))

;; sample
;; [1518-06-12 00:00] Guard #3359 begins shift


;; sort by timestamp
;; year-month-day hour:minute

(defn split-input [s]
  (let [[l r] (map #(apply str %) (split-at 18 s))
        timestamp (subs l 1 17)
        msg (clojure.string/trim r)]
    [timestamp msg]))

(defn sort-data [data]
  (vec (sort (map split-input data))))


;; [1518-02-25 23:56 Guard #1201 begins shift]
;; [1518-02-26 00:14 falls asleep]
;; [1518-02-26 00:30 wakes up]
;; [1518-02-26 00:37 falls asleep]
;; [1518-02-26 00:42 wakes up]
;; [1518-02-26 00:50 falls asleep]
;; [1518-02-26 00:53 wakes up]
;; [1518-02-26 23:52 Guard #3359 begins shift]
;; [1518-02-27 00:05 falls asleep]
;; [1518-02-27 00:12 wakes up]
;; [1518-02-27 00:19 falls asleep]
;; [1518-02-27 00:24 wakes up]
;; [1518-02-27 23:58 Guard #1973 begins shift]
;; [1518-02-28 00:26 falls asleep]
;; [1518-02-28 00:55 wakes up]


;; Begin shift messages need to round up to the next day. 
;; Falls asleep on the minute. Wake up just before the minute

;; Which guard slept the most total
;; Which hour did he sleep the most (across days)


;; Guards shift
;; [1518-11-23 00:03 Guard #3019 begins shift]
;; [1518-11-23 00:17 falls asleep]
;; [1518-11-23 00:46 wakes up]
;; [1518-11-23 00:53 falls asleep]
;; [1518-11-23 00:56 wakes up]

;; {:date "11-23"
;;  :guard "3019"
;;  :sleep [[17 46] [53 56]]}

(defn sleep-hours [guard]
  (flatten (mapv (fn [[s e]] (vec (range s e))) (guard :sleep))))

;; (defn guard-row [data]
;;   (.contains (second data) "Guard"))

;; (["1518-02-25 23:56" "Guard #1201 begins shift"])
;; (["1518-02-26 00:14" "falls asleep"] ["1518-02-26 00:30" "wakes up"] ["1518-02-26 00:37" "falls asleep"] ["1518-02-26 00:42" "wakes up"] ["1518-02-26 00

(defn partition-by-guard-data [data]
  (letfn [(guard-row [data] (.contains (second data) "Guard"))]
    (vec (partition-by guard-row data))))
  


(defn parse-guard-id [g]
  (Integer. (second (clojure.string/split (nth (clojure.string/split (second (first g) ) #" ") 1) #"\#"))))


(defn parse-guard-data-date [d]
  (let [[_ m d _] (clojure.string/split  (first (first d)) #"-")]
    (str m "-" (first (clojure.string/split d #" ")))))




(defn parse-guard-data-sleep [d]
  (let [[_ _ t] (clojure.string/split  (first d) #" |:")
        start (= "falls" (first (clojure.string/split (second d) #" ")))]
    ;; (second (clojure.string/split t #":"))
    ;;[(Integer. t) start]
    (Integer. t)
        )
    )

(defn build-sleep-list [d]
  (flatten (map (fn [[s e]] (range s e)) (partition 2 (map parse-guard-data-sleep d)))))


(defn process-guard-info [partitioned-data]
  (let [g (first partitioned-data)
        d (second partitioned-data)]
    {:id (parse-guard-id g)
     :date (parse-guard-data-date d)
     :sleep (build-sleep-list d)
     }))

(defn add-cnt [h id cnt]
  (assoc h id (+ cnt (get h id 0))))

(defn slept-most [data]
  (loop [data data
         acc {}]
    (if (empty? data)
      acc
      ;; get the entries sleep count and update counter in return map
      (let [d (first data)
            id (:id d)
            cnt (count (:sleep d))]
        ;; update acc
        (recur (next data) (add-cnt acc id cnt))))))

(defn slept-most-id [data]
  (key (apply max-key val (slept-most data))))   ;; 421


;; for a given id, which hour was slept in the most
(defn get-hour [data id]
  (first (apply max-key val (frequencies (flatten (map :sleep (filter #(= id (:id %)) data) )))))     ;; [27 14])
)


;; advent.day04.repose> (* 421 27)
;; 11367

(defn process [partitioned-data]
  (loop [data partitioned-data
         acc []]
    (if (empty? data)
      acc
      (recur (rest (rest data)) (conj acc (process-guard-info data))))))


(defn part1 []
  (let [data (sort-data (read-input))
        partitioned-data (partition-by-guard-data data)
        processed-data (process partitioned-data)]
    (let [id (slept-most-id processed-data)
          hour (get-hour processed-data id)]
      (* id hour))))


;; Find the guard who has slept the most minutes. Then find the minute where the guard 
;; was asleep the most. Multiple the id by the minute to get the answer

;; Id = 421
;; Minute = 27

(comment
  (part1)   ;; 11367
)




;; ----------------------------------------------------------------------------------------------------
;; part2

