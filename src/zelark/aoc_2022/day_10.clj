(ns zelark.aoc-2022.day-10
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 10: Cathode-Ray Tube ---
;; https://adventofcode.com/2022/day/10

(def input (aoc/get-input 2022 10))

(defn parse-input [input]
  (->> (str/replace input #"noop|addx" "0")
       (aoc/parse-longs)
       (reductions + 1))) ; add 1 as init state of register x.

;; part 1
(let [xs (vec (cons 0 (parse-input input)))] ; add zero to align with cycles
  (->> [20 60 100 140 180 220]
       (map #(* % (xs %)))
       (reduce +))) ; 11720

;; part 2
(defn ->px [crt-pos sprite-pos]
  (let [visible? (<= -1 (- crt-pos sprite-pos) 1)]
    (if visible? \# \.)))

(->> (parse-input input)
     (map ->px (cycle (range 40)))
     (partition 40)
     (map #(str/join %))) ; ERCREPCJ
