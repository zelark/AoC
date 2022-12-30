(ns zelark.aoc-2022.day-16
  (:require [clojure.string :as str]
            [zelark.aoc.core :as aoc]
            [zelark.aoc.graph :as g]))

;; --- Day 16: Proboscidea Volcanium ---
;; https://adventofcode.com/2022/day/16

(def input (aoc/get-input 2022 16))

(defn parse-line [line]
  (let [[valve & neighbors] (re-seq #"\p{Upper}\p{Upper}" line)
        flow-rate (parse-long (re-find #"\d+" line))]
    {:valve valve
     :flow-rate flow-rate
     :neighbors (vec neighbors)}))

(defn parse [input]
  (->> (str/split-lines input)
       (map parse-line)
       (reduce (fn [m {:keys [valve neighbors flow-rate]}]
                 (-> m
                     (assoc-in [:valves valve] neighbors)
                     (assoc-in [:flow-rate valve] flow-rate)))
               {})))

(defn solve [input max-time]
  (let [{:keys [valves flow-rate]} (parse input)
        cost (g/shortest-paths valves)
        target-valves (filter #(-> % flow-rate pos?) (keys valves)) ; Valves with positive rate flow.
        valve->power (zipmap target-valves
                             (map #(bit-shift-left 1 %) (range)))
        max-pressure (fn max-pressure [time pressure valve ks answer]
                       (let [answer' (update answer ks (fnil max 0) pressure)
                             answers (for [target target-valves
                                           :let   [time-required (inc (cost [valve target]))
                                                   time-left     (- time time-required)]
                                           :when  (and (zero? (bit-and ks (valve->power target))) ; Not yet opened, and
                                                       (< time-required time))]                   ; time enough.
                                       (max-pressure time-left
                                                     (+ pressure (* (get flow-rate target) time-left))
                                                     target
                                                     (bit-or ks (valve->power target))
                                                     answer'))]
                         (if (seq answers)
                           (apply merge-with max answers)
                           answer')))]
    (max-pressure max-time 0 "AA" 0 {})))

;; part 1
(->> (solve input 30)
     (vals)
     (apply max)) ; => 2183

;; part 2
(let [answer (solve input 26)]
  (->> (for [[k1 v1] answer
             [k2 v2] answer
             :when (zero? (bit-and k1 k2))] ; Only inverted keys make sense. 1010 and 0101 are inverted,
         (+ v1 v2))                         ; that is the elephant and me opened different valves.
       (apply max))) ; => 2911              ; Then we add their values (max pressure released), and find a maximum one.
