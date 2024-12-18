(ns zelark.aoc-2024.day-18
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [zelark.aoc.grid-2d :as g2]
            [zelark.aoc.graph :as g]))

;; --- Day 18: RAM Run ---
;; https://adventofcode.com/2024/day/18

(def input (aoc/get-input 2024 18))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map aoc/parse-longs)))

(defn neighbors [boundaries bytes]
  (fn [loc]
    (->> (g2/neighbors loc)
         (filter #(g2/in-bounds? boundaries %))
         (remove bytes))))

;; part 1 (22.928986 msecs)
(let [bytes      (zipmap (take 1024 (parse-input input))
                         (repeat \#))
      boundaries [[0 0] [70 70]]
      neighbors  (neighbors boundaries bytes)
      steps (g/bfs neighbors [0 0] [70 70])]
  (dec (count steps))) ; 312

;; part 2 (15475.939353 msecs)
(time (let [bytes (parse-input input)
            grid  (zipmap (take 1024 bytes) (repeat \#))
            boundaries [[0 0] [70 70]]]
        (->> (reduce (fn [g byte]
                       (let [grid (assoc g byte \#)]
                         (if (seq (g/bfs (neighbors boundaries grid) [0 0] [70 70]))
                           grid
                           (reduced byte))))
                     grid
                     (drop 1024 bytes))
             (str/join ",")))) ; 28,26
