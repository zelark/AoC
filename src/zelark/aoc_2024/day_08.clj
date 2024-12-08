(ns zelark.aoc-2024.day-08
  (:require [zelark.aoc.core :as aoc]
            [clojure.math.combinatorics :as combo]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 8: Resonant Collinearity ---
;; https://adventofcode.com/2024/day/8

(def input (aoc/get-input 2024 8))

(defn parse-input [input]
  (let [grid     (g2/parse input)
        bounds   (g2/boundaries grid)
        antennas (->> (filter #(not (g2/empty-space? (val %))) grid)
                      (keys)
                      (group-by grid))]
    {:bounds   bounds
     :antennas antennas}))

(defn solve [antinodes input]
  (let [{:keys [bounds antennas]} (parse-input input)
        in-bounds? (partial g2/in-bounds? bounds)]
    (->> (vals antennas)
         (mapcat #(combo/combinations % 2))
         (mapcat #(antinodes in-bounds? %))
         (set)
         (count))))

;; part 1 (7.379424 msecs)
(defn part1 [in-bounds? [a b]]
  (let [[dx dy] (g2/minus a b)]
    (->> [(g2/plus a [dx dy]) (g2/minus b [dx dy])]
         (filter in-bounds?))))

(solve part1 input) ; 329

;; part 2 (9.424784 msecs)
(defn part2 [in-bounds? [a b]]
  (let [[dx dy] (g2/minus a b)]
    (concat
     (take-while in-bounds? (iterate #(g2/plus  % [dx dy]) a))
     (take-while in-bounds? (iterate #(g2/minus % [dx dy]) b)))))

(solve part2 input) ; 1190
