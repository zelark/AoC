(ns zelark.aoc-2023.day-02
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 2: Cube Conundrum ---
;; https://adventofcode.com/2023/day/2

(def input (aoc/get-input 2023 02))

(defn parse-cubes [line]
  (->> (re-seq #"(\d+) (red|green|blue)" line)
       (reduce (fn [m [_ n color]]
                 (assoc m (keyword color) (parse-long n)))
               {})))

(defn prepare [games]
  (reduce-kv (fn [m id cubes]
               (assoc m id (reduce #(merge-with max %1 %2) cubes)))
             {}
             games))

(defn possible?
  "Only 12 red cubes, 13 green cubes, and 14 blue cubes."
  [{:keys [red green blue] :or {red 0 green 0 blue 0}}]
  (and (<= red   12)
       (<= green 13)
       (<= blue  14)))

(defn solve [f input]
  (->> (aoc/parse-clines input #";" parse-cubes)
       (prepare)
       (reduce-kv f 0)))

;; part 1
(defn part1 [acc id cubes]
  (if (possible? cubes) (+ acc id) acc))

(solve part1 input) ; 2076

;; part 2
(defn part2 [acc _ cubes]
  (+ acc (apply * (vals cubes))))

(solve part2 input) ; 70950
