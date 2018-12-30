(ns advent.day16.chronal
  (:require [clojure.string :as str]))


(def pp clojure.pprint/pprint)

(defn parse-state
  "Parse  `Before: [1 2 3 4]` into `[1 2 3 4]`"
  [s]
  (let [[_ v] (str/split s #"\[")
        len (count v)
        l (.substring v 0 (dec len))]
    (mapv #(Integer/parseInt %) (str/split l #", "))))

(defn parse-inst
  "Parse the string with 4 integers into a vector of 4 integer"
  [s]
  (mapv #(Integer/parseInt %) (str/split s #" ")))

(defn load-samples
  "Read the four line samples into three vectors"
  []
  (let [input-file "resources/day16/input1.txt"
        input-lines (str/split-lines (slurp input-file))]
    (map (fn [[b i a _]] [(parse-state b) (parse-inst i) (parse-state a)]) (partition 4 input-lines))))


;; ----------------------------------------------------------------------------------------------------
;; Opcodes

;; Addition:

;;    addr (add register) stores into register C the result of adding register A and register B.
(defn addr [state a b c]
  (assoc state c (+ (state a) (state b))))

;;    addi (add immediate) stores into register C the result of adding register A and value B.
(defn addi [state a b c]
  (assoc state c (+ (state a) b)))

;; Multiplication:

;;     mulr (multiply register) stores into register C the result of multiplying register A and register B.
(defn mulr [state a b c]
  (assoc state c (* (state a) (state b))))

;;     muli (multiply immediate) stores into register C the result of multiplying register A and value B.
(defn muli [state a b c]
  (assoc state c (* (state a) b)))

;; Bitwise AND:

;;     banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
(defn banr [state a b c]
  (assoc state c (bit-and (state a) (state b))))

;;     bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
(defn bani [state a b c]
  (assoc state c (bit-and (state a) b)))


;; Bitwise OR:

;;     borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
(defn borr [state a b c]
  (assoc state c (bit-or (state a) (state b))))

;;     bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
(defn bori [state a b c]
  (assoc state c (bit-or (state a) b)))


;; Assignment:

;;     setr (set register) copies the contents of register A into register C. (Input B is ignored.)
(defn setr [state a _ c]
  (assoc state c (state a)))

;;     seti (set immediate) stores value A into register C. (Input B is ignored.)
(defn seti [state a _ c]
  (assoc state c a))


;; Greater-than testing:

;;     gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
(defn gtir [state a b c]
  (assoc state c (if (> a (state b)) 1 0)))

;;     gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
(defn gtri [state a b c]
  (assoc state c (if (> (state a) b) 1 0)))
;;     gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
(defn gtrr [state a b c]
  (assoc state c (if (> (state a) (state b)) 1 0)))

;; Equality testing:

;;     eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
(defn eqir [state a b c]
  (assoc state c (if (= a (state b)) 1 0)))

;;     eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
(defn eqri [state a b c]
  (assoc state c (if (= (state a) b) 1 0)))

;;     eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
(defn eqrr [state a b c]
  (assoc state c (if (= (state a) (state b)) 1 0)))


(def opcodes [addr addi mult muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])



;; ----------------------------------------------------------------------------------------------------
;; Testing

;; Before: [3, 2, 1, 1]
;; 9 2 1 2
;; After:  [3, 2, 2, 1]


(def test-sample [[3 2 1 1] [9 2 1 2] [3 2 2 1]])

;; advent.day16.chronal> (test-opcodes test-sample)
;; 3


;; ----------------------------------------------------------------------------------------------------
;; Part 1
;; How many samples match the operations of three or more opcodes

(defn test-opcode 
  ([opcode [before inst after]]
   (test-opcode opcode before inst after))
  ([opcode before inst after]
   (= (apply opcode before (rest inst)) after)))

(defn test-opcodes [opcodes sample]
  (let [[before inst after] sample]
    (count (filter #(test-opcode % before inst after) opcodes))))

(defn part1 []
  (count (filter #(> % 2) (map #(test-opcodes opcodes %) (load-samples)))))


;; advent.day16.chronal> (part1)
;; 646


;; ----------------------------------------------------------------------------------------------------
;; Part 2
;; Deduce the opcode numbers and then run the sample program (input2.txt)

(defn load-program
  "Read input2.txt and convert to instructions"
  []
  (let [input-file "resources/day16/input2.txt"
        input-lines (str/split-lines (slurp input-file))]
    (map #(parse-inst %) input-lines)))


(defn run-program [instructions opcode-map]
  (reduce (fn [state [op a b c]]
            ((opcode-map op) state a b c))
          [0 0 0 0]
          instructions))


(defn op-set [acc ops [before [opcode & args] after]]
  (reduce
    (fn [m op]
      (if (= (apply op before args) after)
        (update m opcode (fnil conj #{}) op)
        m))
    acc
    ops))

(defn num->ops [samples ops]
  (loop [samples samples
         acc {}]
    (if (empty? samples)
      acc
      (recur (rest samples) (op-set acc ops (first samples))))))


(defn find-singles [m]
  (filter (fn [[num ops]] (= (count ops) 1)) m))


(defn deduce
  "num->ops returns map with keys which are operand number and value a list of potential instructions.
Figure out which num goes to which function. Start by filtering 'known' values. Those with single match
"
  [samples opcodes]
  (loop [acc {}
         unassigned (into #{} opcodes)]
    ;; (pp acc)
    ;; (pp unassigned)
    (if (empty? unassigned)
      acc
      (let [ops-map (num->ops samples (vec unassigned))
            singles (find-singles ops-map)
            single (first singles)
            opcode (first single)
            fnc (first (second single))]
        (recur (assoc acc opcode fnc) (disj unassigned fnc))
        ))))


(defn part2 []
  (let [op-map (deduce (load-samples) opcodes)
        program (load-program)]
    (let [[r0 r1 r2 r3] (run-program program op-map)]
      r0)))


(comment
  (part2)    ;; 681
)
