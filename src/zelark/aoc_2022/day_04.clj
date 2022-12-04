(ns zelark.aoc-2022.day-04
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 4: Camp Cleanup ---
;; https://adventofcode.com/2022/day/4

(def input (aoc/get-input 2022 04))

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (map parse-long)
       (partition 2)
       (partition 2)))

(defn fully-contains? [[start-a end-a] [start-b end-b]]
  (<= start-a start-b end-b end-a))

(defn overlap? [[start-a end-a] [start-b end-b]]
  (and (<= start-a end-b)
       (<= start-b end-a)))

(defn solve [input pred]
  (->> (parse-input input)
       (filter pred)
       (count)))

;; part 1
(solve input (fn [[a b]]
               (or (fully-contains? a b)
                   (fully-contains? b a)))) ; 530

;; part 2
(solve input (fn [[a b]]
               (overlap? a b))) ; 903
