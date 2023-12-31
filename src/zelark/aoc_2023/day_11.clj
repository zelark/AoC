(ns zelark.aoc-2023.day-11
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [clojure.math.combinatorics :as comb]))

;; --- Day 11: Cosmic Expansion ---
;; https://adventofcode.com/2023/day/11

(def input (aoc/get-input 2023 11))

(defn spaces [xs x1 x2]
  (->> (range (inc (min x1 x2)) (max x1 x2))
       (reduce (fn [n x] (if (xs x) n (inc n))) 0)))

(defn solve [input t]
  (let [t (dec t)
        galaxies (keys (g2/parse input #(= % \#)))
        xs (set (map first galaxies))
        ys (set (map second galaxies))]
    (->> (comb/combinations galaxies 2)
         (map (fn [[[x1 y1] [x2 y2]]]
                (let [xx (spaces xs x1 x2)
                      yy (spaces ys y1 y2)]
                  (+ (aoc/manhattan-distance [x1 y1] [x2 y2])
                     (* (+ xx yy) t)))))
         (aoc/sum))))

;; part 1 (397.999698 msecs)
(time (solve input 2)) ; 9608724

;; part 2 (391.411097 msecs)
(time (solve input 1000000)) ; 904633799472
