(ns zelark.aoc-2018.day-01
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 1: Chronal Calibration ---
;; https://adventofcode.com/2018/day/01

(def input (aoc/get-input 2018 01))

(defn parse [input]
  (aoc/parse-longs input))

;; part 1
(->> (parse input)
     (aoc/sum)) ; => 587

;; part 2
(->> (parse input)
     (cycle)
     (reductions +)
     (reduce #(if (%1 %2) (reduced %2) (conj %1 %2)) #{})) ; => 83130
