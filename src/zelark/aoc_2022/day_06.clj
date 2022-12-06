(ns zelark.aoc-2022.day-06
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 6: Tuning Trouble ---
;; https://adventofcode.com/2022/day/6

(def input (aoc/get-input 2022 06))

(defn different? [xs]
  (= (count xs)
     (count (set xs))))

(defn find-marker [n input]
  (->> (partition n 1 input)
       (take-while (complement different?))
       (count)
       (+ n)))

;; part 1
(find-marker 4 input) ; 1965

;; part 2
(find-marker 14 input) ; 2773
