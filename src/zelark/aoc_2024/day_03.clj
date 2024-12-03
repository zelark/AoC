(ns zelark.aoc-2024.day-03
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 3: Mull It Over ---
;; https://adventofcode.com/2024/day/3

(def input (aoc/get-input 2024 3))

(defn parse-input [input]
  (->> (re-seq #"mul\(\d+,\d+\)" input)
       (map #(aoc/parse-longs %))))

(defn solve [input]
  (->> (parse-input input)
       (reduce (fn [acc [x y]] (+ acc (* x y))) 0)))

;; part 1 (1.766576 msecs)
(solve input) ; 161289189

;; part 2 (1.750508 msecs)
(defn prepare-input [input]
  (str/replace input #"(?s)don't\(\).*?(?:do\(\)|\Z)" ""))

(-> (prepare-input input)
    (solve)) ; 83595109
