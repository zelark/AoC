(ns zelark.aoc-2024.day-06
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 6: Guard Gallivant ---
;; https://adventofcode.com/2024/day/6

(def input (aoc/get-input 2024 6))

(def directions {\^ g2/up \v g2/down \> g2/right \< g2/left})

(defn find-guard [grid]
  (some (fn [[loc ch]] (when-let [d (directions ch)] [loc d])) grid))

(defn step [grid [pos dir]]
  (let [new-pos (g2/plus pos dir)]
    (when-let [something (get grid new-pos)]
      (if (g2/empty-space? something)
        [new-pos dir]
        [pos (g2/rotate dir :right)]))))

(defn guard-path [grid start+dir]
  (->> (iterate #(step grid %) start+dir)
       (take-while some?)))

;; part 1 (23.453246 msecs)
(let [grid        (g2/parse input)
      [start dir] (find-guard grid)]
  (->> (guard-path (assoc grid start g2/empty-space) [start dir])
       (into #{} (map first))
       (count))) ; 5242

;; part 2 (6360.225645 msecs)
(defn in-a-loop? [grid [start dir] block-position]
  (let [grid' (assoc grid block-position \O)]
    (->> (guard-path grid' [start dir])
         (reduce #(if (contains? %1 %2) (reduced true) (conj %1 %2)) #{})
         (true?))))

(let [grid        (g2/parse input)
      [start dir] (find-guard grid)
      grid        (assoc grid start g2/empty-space)
      positions   (->> (guard-path (assoc grid start g2/empty-space) [start dir])
                       (into #{} (map first)))]
  (->> (pmap #(in-a-loop? grid [start dir] %) (disj positions start))
       (filter true?)
       (count))) ; 1431
