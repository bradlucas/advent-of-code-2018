(ns advent.day25.constellations
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))

;; The following routines are a slight reworking of day25 over at:
;; https://github.com/thegeez/clj-advent-of-code-2018/blob/master/src/advent2018/core3.clj

(defn load-stars [in]
  (into []
        (map (fn [line]
               (let [[a b c d] (-> line
                                   (str/trim)
                                   (str/split #",")
                                   (->> (map #(Long/parseLong %))))]
                 [a b c d])))
        (str/split-lines (slurp (if (string? in) (java.io.StringReader. in) in)))))

(defn reachable [[^long la ^long lb ^long lc ^long ld] [^long ra ^long rb ^long rc ^long rd]]
  (<= (+ (Math/abs (- la ra))
         (Math/abs (- lb rb))
         (Math/abs (- lc rc))
         (Math/abs (- ld rd)))
      3))

(defn part1 []
  (let [in (io/resource "day25/input.txt")
        stars (load-stars in)
        consts (reduce
                (fn [consts star]
                  (let [join (for [c consts
                                   s c
                                   :when (reachable star s)]
                               c)
                        rem (reduce
                             disj
                             consts
                             join)
                        add (reduce
                             into
                             #{star}
                             join)]
                    (conj rem add)))

                #{}
                stars)]
    (count consts)))


(comment
  (part1)
  ;; => 396
  )
