(ns zelark.aoc-2023.day-17
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 17: Clumsy Crucible ---
;; https://adventofcode.com/2023/day/17

(def input (aoc/get-input 2023 17))

(defn dist [m from to]
  (when (and (m from) (m to))
    (reduce (fn [acc loc] (+ acc (m loc))) 0 (rest (g2/line-points from to)))))

(defn next-blocks [city-map min max crucible]
  (let [[loc dir] crucible
        dirs (condp = dir
               g2/right [g2/up    g2/down]
               g2/left  [g2/up    g2/down]
               g2/up    [g2/left  g2/right]
               g2/down  [g2/left  g2/right]
                        [g2/right g2/down])] ; Start point is always [0 0].
    (->> (for [d dirs]
           (->> (iterate (fn [[a b]] [(g2/plus a b) b]) [loc d])
                (take (inc max))
                (drop min)))
         (mapcat identity)
         (filter #(city-map (first %))))))

(defn solve [input min max]
  (let [city-map    (g2/parse input identity aoc/ch->digit)
        [sloc gloc] (g2/boundaries city-map)
        start       [sloc nil]
        goal?       #(= (first %) gloc)
        dist        #(dist city-map (first %1) (first %2))
        h           #(aoc/manhattan-distance (first %) gloc)
        next-blocks #(next-blocks city-map min max %)]
    (aoc/astar next-blocks dist h start goal? {:score? true})))

;; part 1
(solve input 1 3) ; 1008

;; part 2
(solve input 4 10) ; 1210
