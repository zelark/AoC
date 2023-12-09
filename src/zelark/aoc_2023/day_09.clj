(ns zelark.aoc-2023.day-09
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 9: Mirage Maintenance ---
;; https://adventofcode.com/2023/day/9

(def input (aoc/get-input 2023 9))

(defn parse-input [input] 
  (->> (str/split-lines input)
       (mapv aoc/parse-longs)))

(defn extrapolate [orig-seq & {:keys [backward?]}]
  (let [[pull op] (if backward? [first -] [peek +])]
    (->> (iterate (fn [xs] (mapv - (rest xs) xs)) orig-seq)
         (aoc/take-until #(every? zero? %))
         (map pull)
         (reverse)
         (reduce (fn [acc n] (op n acc))))))

;; part 1
(->> (parse-input input)
     (map extrapolate)
     (aoc/sum)) ; 1782868781

;; part 2
(->> (parse-input input)
     (map #(extrapolate % :backward? true))
     (aoc/sum)) ; 1057
