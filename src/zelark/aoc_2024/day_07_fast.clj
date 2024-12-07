(ns zelark.aoc-2024.day-07-fast
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 7: Bridge Repair ---
;; https://adventofcode.com/2024/day/7

;; Inspired by @erdos's solution:
;; https://github.com/erdos/advent-of-code/blob/master/2024/day07.clj

(def input (aoc/get-input 2024 7))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map aoc/parse-longs)))

(defn cut-off [x y]
  (let [s1 (str x)
        s2 (str y)]
    (when (str/ends-with? s1 s2)
      (parse-long (subs s1 0 (- (count s1) (count s2)))))))

(defn can-be-true? [value [number & numbers] cat?]
  (if (seq numbers)
    (or (when (zero? (rem value number))
          (can-be-true? (quot value number) numbers cat?))
        (when (> value number)
          (can-be-true? (- value number) numbers cat?))
        (when cat?
          (some-> (cut-off value number) (recur numbers cat?))))
    (= value number)))

(defn solve [part input]
  (->> (parse-input input)
       (filter #(can-be-true? (first %) (reverse (rest %)) (= part :part2)))
       (aoc/sum first)))

;; part 1 (4.9716 msecs)
(solve :part1 input) ; 4555081946288

;; part 2 (6.166713 msecs)
(solve :part2 input) ; 227921760109726
