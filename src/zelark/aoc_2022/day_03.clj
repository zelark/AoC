(ns zelark.aoc-2022.day-03
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 3: Rucksack Reorganization ---
;; https://adventofcode.com/2022/day/3

(def input (aoc/get-input 2022 03))

(defn ->priority [item-type]
  (cond
    (Character/isLowerCase item-type) (- (int item-type) 96)
    (Character/isUpperCase item-type) (- (int item-type) 38)))

(defn parse-input [input]
  (str/split-lines input))

;; part 1
(defn find-common-p1 [rucksack]
  (let [n (count rucksack)
        [left-half right-half] (split-at (/ n 2) rucksack)]
    (set/intersection (set left-half) (set right-half))))

(->> (parse-input input)
     (transduce (comp (mapcat find-common-p1)
                      (map ->priority))
                +)) ; 7990

;; part 2
(defn find-common-p2 [rucksacks]
  (apply set/intersection (map set rucksacks)))

(->> (parse-input input)
     (transduce (comp (partition-all 3)
                      (mapcat find-common-p2)
                      (map ->priority))
                +)) ; 2602
