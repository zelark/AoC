(ns zelark.aoc-2023.day-09
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [medley.core :as mdl]))

;; --- Day 9: Mirage Maintenance ---
;; https://adventofcode.com/2023/day/9

(def input (aoc/get-input 2023 9))

(defn parse-input [input] 
  (->> (str/split-lines input)
       (mapv aoc/parse-longs)))

(defn extrapolate [orig-seq]
  (->> (reverse orig-seq)
       (iterate (fn [xs] (map - xs (rest xs))))
       (mdl/take-upto #(every? zero? %))
       (aoc/sum first)))

;; part 1 (8.826545 msecs)
(->> (parse-input input)
     (aoc/sum extrapolate)) ; 1782868781

;; part 2 (8.690005 msecs)
(->> (parse-input input)
     (map reverse)
     (aoc/sum extrapolate)) ; 1057
