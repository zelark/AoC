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

(defn neighbors [boundaries corrupted?]
  (fn [loc]
    (->> (g2/neighbors loc)
         (filter #(g2/in-bounds? boundaries %))
         (remove corrupted?))))

(defn shortest-path [bytes n start end]
  (let [corrupted? (set (take n bytes))
        boundaries [start end]]
    (g/bfs (neighbors boundaries corrupted?) start end)))

(defn solve [part input]
  (let [bytes     (parse-input input)
        find-path #(shortest-path bytes % [0 0] [70 70])]
    (if (= part :p1)
      (dec (count (find-path 1024)))
      (->> (aoc/binary-search find-path (count bytes))
           (nth bytes)
           (str/join ",")))))

;; part 1 (22.928986 msecs)
(solve :p1 input) ; 312

;; part 2 (52.361474 msecs)
(solve :p2 input) ; 28,26
