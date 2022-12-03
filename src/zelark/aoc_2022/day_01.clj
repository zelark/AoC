(ns zelark.aoc-2022.day-01
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 1: Calorie Counting ---
;; https://adventofcode.com/2022/day/1

(def input (aoc/get-input 2022 01))

(defn parse-input [input]
  (->> (aoc/split-on-blankline input)
       (map aoc/parse-longs)))

(def calories (map aoc/sum (parse-input input)))

;; part 1
(->> calories (apply max)) ; 68802

;; part 2
(->> calories (sort >) (take 3) aoc/sum) ; 205370
