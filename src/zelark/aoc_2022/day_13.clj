(ns zelark.aoc-2022.day-13
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 13: Distress Signal ---
;; https://adventofcode.com/2022/day/13

(def input (aoc/get-input 2022 13))

(defn parse-pair [p]
  (->> (str/split-lines p)
       (mapv edn/read-string)))

(defn parse-input [input]
  (->> (aoc/split-on-blankline input)
       (map parse-pair)))

(defn cmp [[left & left-rest] [right & right-rest]]
  (cond
    (every? nil? [left right]) nil
    (nil? left)  true
    (nil? right) false

    (every? number? [left right])
    (if (= left right)
      (recur left-rest right-rest)
      (< left right))

    (every? vector? [left right])
    (if-some [ret (cmp left right)]
      ret
      (recur left-rest right-rest))

    (number? left)  (recur (cons [left] left-rest) (cons right right-rest))
    (number? right) (recur (cons left left-rest)   (cons [right] right-rest))

    :else (recur left-rest right-rest)))

;; part 1
(->> (parse-input input)
     (keep-indexed #(when (apply cmp %2) (inc %1)))
     (aoc/sum)) ; 5350

;; part 2
(let [packets (mapcat identity (parse-input input))
      d2 [[2]]
      d6 [[6]]
      delimiter? #{d2 d6}]
  (->> (into packets [d2 d6])
       (sort cmp)
       (keep-indexed #(when (delimiter? %2) (inc %1)))
       (aoc/mul))) ; 19570
