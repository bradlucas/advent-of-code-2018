(ns advent.day24.immune
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]))


;; https://github.com/thegeez/clj-advent-of-code-2018/blob/master/src/advent2018/core3.clj
;; https://github.com/borkdude/advent-of-cljc/tree/master/src/aoc/y2018/d24


;; @see https://github.com/armstnp/advent-of-code-2018/blob/master/clojure/src/advent_of_code_2018/day24.clj


(defn load-input []
  (str/split-lines (slurp "resources/day24/input.txt")))


(defn split-input [in]
  (let [[[_ & immune-lines] [_ _ & infection-lines]] (split-with #(not (empty? %)) (load-input))]
    {:immune immune-lines
     :infection infection-lines}))


;; arimes -> groups -> units


;; The immune system and the infection each have an army made up of
;; several groups; each group consists of one or more identical
;; units. The armies repeatedly fight until only one army has units
;; remaining.


;; Units within a group:
;; hit points
;; attack damage
;; attack type
;; initiative
;; weakness
;; immunities
;; effective power

;; UNIT
;; hit points
;; attack damage
;; attack type
;; initiative (hiehger attack first and win ties)
;; weakness or immunities

;; GROUP
;; effective power == number of units * attack damage


;; Example group
;; 18 units each with 729 hit points (weak to fire; immune to cold, slashing)  with an attack that does 8 radiation damage at initiative 10
;; UNITS unites each with HIT-POINTS hit points (WEAKNESSES, IMMUNITIES) with an attach that does XXX at initiative INITIATIVE

;; {:units 10
;;  :hit-points 729
;;  :attack-damage 8
;;  :attack-type :radiation
;;  :initiative 10
;;  :weaknesses [:fire]
;;  :immunities [:cold :slashing]
;; }

(defn parse-characteristics [re s]
  (set
   (some->> s
            (re-find re)
            second
            (#(str/split % #", "))
            (map keyword))))

(defn parse-group [id line]
  (let [[_ units-str hit-points-str characteristics attack-damage-str attack-type initiative-str]
        (re-matches #"(\d+) units each with (\d+) hit points (.+)?with an attack that does (\d+) (.+) damage at initiative (\d+)" line)
        [units hit-points attack-damage initiative] (map #(Integer/parseInt %) [units-str hit-points-str attack-damage-str initiative-str])
        weaknesses (parse-characteristics  #"weak to (.+?)(?:;|\))" characteristics)
        immunities (parse-characteristics  #"immune to (.+?)(?:;|\))" characteristics)]
    {:id id
     :unit-count units
     :hit-points hit-points
     :attack-damage attack-damage
     :attack-type (keyword attack-type)
     :weaknesses weaknesses
     :immunities immunities
     :initiative initiative}))

(defn parse-army [army lines]
  (map (fn [m] (assoc m :army (keyword army) :id (format "%s-%s" army (:id m)))) (map-indexed parse-group lines)))

;; (concat (parse-army (name :immune) (:immune (part1))) (parse-army (name :infection) (:infection (part1))))

(defn parse-armies [input]
  (->> (concat (parse-army (name :immune) (:immune input)) (parse-army (name :infection) (:infection input)))
       (map #(vector (:id %) %))
       (into {})))

;; fight consists of two phases: target selection and attacking

;; ----------------------------------------------------------------------------------------------------
;; The following routines are slight modifications of routines at:
;; https://github.com/armstnp/advent-of-code-2018/blob/master/clojure/src/advent_of_code_2018/day24.clj
;; ----------------------------------------------------------------------------------------------------

(defn enemies? [{army-a :army} {army-b :army}]
  (not= army-a army-b))

(defn weak? [{:keys [weaknesses]} attack-type]
  (contains? weaknesses attack-type))

(defn immune? [{:keys [immunities]} attack-type]
  (contains? immunities attack-type))

(defn effective-power [{:keys [unit-count attack-damage]}]
  (* ^int unit-count ^int attack-damage))

(defn outgoing-attack-damage [{:keys [attack-type] :as attacker} defender]
  (cond
    (immune? defender attack-type) 0
    (weak? defender attack-type) (* 2 ^int (effective-power attacker))
    :else (effective-power attacker)))

;; Target Selection

(defn sort-targeting [groups]
  (sort-by (fn [{:keys [initiative] :as group}]
             [(- ^int (effective-power group))
              (- ^int initiative)])
           groups))

(defn select-target [attacker targets]
  (->> targets
       (filter #(and (enemies? attacker %)
                     (> ^int (outgoing-attack-damage attacker %) 0)))
       (sort-by #(vector (- ^int (outgoing-attack-damage attacker %))
                         (- ^int (effective-power %))
                         (- ^int (:initiative %))))
       first))

(defn bind-target [{:keys [defenders bindings] :as selection-state} attacker]
  (if-let [target (select-target attacker defenders)]
    {:defenders (disj defenders target)
     :bindings (conj bindings {:attacker-id (:id attacker) :defender-id (:id target)})}
    selection-state))

(defn select-targets [groups]
  (let [attackers (sort-targeting groups)
        defenders (set groups)]
    (->> attackers
         (reduce bind-target {:defenders defenders :bindings []})
         :bindings)))

;; Attacking

(defn attack [attacker {:keys [unit-count hit-points] :as defender}]
  (let [attack-damage (outgoing-attack-damage attacker defender)
        unit-kills (quot ^int attack-damage ^int hit-points)
        new-unit-count (max 0 (- ^int unit-count unit-kills))
        dead? (zero? new-unit-count)]
    (assoc defender
           :unit-count new-unit-count
           :dead? dead?)))

(defn attack-and-update [groups {:keys [attacker-id defender-id]}]
  (let [attacker (groups attacker-id)]
    (update groups defender-id #(attack attacker %))))

(defn run-attack-phase [groups target-bindings]
  (->> target-bindings
       (sort-by #(->> % :attacker-id groups :initiative -))
       (reduce attack-and-update groups)))

;; Battle

(defn battle-over? [groups]
  (->> groups
       vals
       (group-by :army)
       count
       (= 1)))

(defn fight [groups]
  (->> groups
       vals
       select-targets
       (run-attack-phase groups)
       (filter #(not (:dead? (second %))))
       (into {})))

(defn hit-points-remaining [groups]
  (->> groups
       vals
       (map :unit-count)
       (apply +)))

;; ----------------------------------------------------------------------------------------------------
;; Part 1

(defn part1 []
  (->> (load-input)
       split-input
       parse-armies
       (iterate fight)
       (filter battle-over?)
       first
       hit-points-remaining))

(comment
  (part1)
  ;; => 15493
  )


