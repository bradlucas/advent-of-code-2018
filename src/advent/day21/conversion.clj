(ns advent.day21.conversion
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            [advent.day19.register :as day19]))


(def input-file "resources/day21/input.txt")

(defn load-input []
  (let [[line1 & instructions] (str/split-lines (slurp input-file))
        ip (read-string (last (str/split line1 #" ")))
        ]
    [ip (day19/parse-instructions instructions)]))


;; Builds on Day 16 and Day 19

;; ----------------------------------------------------------------------------------------------------

;; From Day 16

(defn addr [state a b c]
  (assoc state c (+ (state a) (state b))))

(defn addi [state a b c]
  (assoc state c (+ (state a) b)))

(defn mulr [state a b c]
  (assoc state c (* (state a) (state b))))

(defn muli [state a b c]
  (assoc state c (* (state a) b)))

(defn banr [state a b c]
  (assoc state c (bit-and (state a) (state b))))

(defn bani [state a b c]
  (assoc state c (bit-and (state a) b)))

(defn borr [state a b c]
  (assoc state c (bit-or (state a) (state b))))

(defn bori [state a b c]
  (assoc state c (bit-or (state a) b)))

(defn setr [state a _ c]
  (assoc state c (state a)))

(defn seti [state a _ c]
  (assoc state c a))

(defn gtir [state a b c]
  (assoc state c (if (> a (state b)) 1 0)))

(defn gtri [state a b c]
  (assoc state c (if (> (state a) b) 1 0)))

(defn gtrr [state a b c]
  (assoc state c (if (> (state a) (state b)) 1 0)))

(defn eqir [state a b c]
  (assoc state c (if (= a (state b)) 1 0)))

(defn eqri [state a b c]
  (assoc state c (if (= (state a) b) 1 0)))

(defn eqrr [state a b c]
  (assoc state c (if (= (state a) (state b)) 1 0)))

(def opcodes [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

(defn apply-opcode [regs op [_ a b c]]
  (case (keyword op)
    :addr (addr regs a b c)
    :addi (addi regs a b c)
    :mulr (mulr regs a b c)
    :muli (muli regs a b c)
    :banr (banr regs a b c) 
    :bani (bani regs a b c)
    :borr (borr regs a b c) 
    :bori (bori regs a b c) 
    :setr (setr regs a b c) 
    :seti (seti regs a b c) 
    :gtir (gtir regs a b c)
    :gtri (gtri regs a b c) 
    :gtrr (gtrr regs a b c) 
    :eqir (eqir regs a b c) 
    :eqri (eqri regs a b c)
    :eqrr (eqrr regs a b c)))

;; @from https://github.com/Average-user/adventofcode-clj-2018/blob/master/src/adventofcode_clj_2018/day19.clj
;;
(defn run [ip-register intructions registers]
    (loop [registers registers
           ip 0]
      (let [i   (get intructions ip)                         ;; next instruction pointer (index)
            registers' (assoc registers ip-register ip)]     ;; load ip
        ;; if next instruction exists
        (if i
          ;; run instruction
          (let [op (first i)
                registers'' (apply-opcode registers' op i)]

            ;; @see https://www.reddit.com/r/adventofcode/comments/a86jgt/2018_day_21_solutions/
            ;; return the first value of r5 when at intruction pointer 28
            (if (= ip 28)
              (do
                (pp/pprint (format "%s %s - %s : %s - %s" ip (nth registers' 5) (nth registers'' 5) registers' registers''))
                (nth registers'' 5))
              (recur registers'' (inc (get registers'' ip-register)))
              ))
          ;; else return final state
          registers))))


;; ;; ----------------------------------------------------------------------------------------------------

;; the following doesn't return
;; others suggest reverse-engineering the program
(defn part1 []
  (let [[ipr instructions] (load-input)]
    ;; (first (run ipr instructions [0 0 0 0 0 0]))
    (run ipr instructions [0 0 0 0 0 0])))


(comment
  (part1)
  ;; => 2884703
)

