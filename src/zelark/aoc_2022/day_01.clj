(ns zelark.aoc-2022.day-01
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 1: Calorie Counting ---
;; https://adventofcode.com/2022/day/1

(def input (aoc/get-input 2022 01))

(defn parse-input [input]
  (map (fn [xs]
          (->> (re-seq #"\d+" xs)
               (transduce (map parse-long) +)))
        (str/split input #"\R\R")))

;; part 1
(->> (parse-input input)
     (apply max)) ; 68802

;; part 2
(->> (parse-input input)
     (sort >)
     (take 3)
     (reduce +)) ; 205370
