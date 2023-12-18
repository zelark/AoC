(ns zelark.aoc-2023.day-18-neat
  (:require [zelark.aoc-2023.day-18 :as day18]))

;; --- Day 18: Lavaduct Lagoon ---
;; https://adventofcode.com/2023/day/18

;; Without extra g2/functions and even trench-points isn't needed.
(defn solve [input & {:keys [correct-plan?]}]
  (->> (cond-> (day18/parse input) correct-plan? day18/correct)
       (reduce (fn [[acc x] [d n]]
                 (let [[dx dy] (day18/direction d)
                       x' (+ x (* dx n))]
                   [(+ acc (* dy n x') (/ n 2)) x']))
               [1 0])
       (first)))

;; part 1
(solve day18/input) ; 48400

;; part 2
(solve day18/input :correct-plan? true) ; 72811019847283
