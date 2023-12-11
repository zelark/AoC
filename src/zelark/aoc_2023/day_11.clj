(ns zelark.aoc-2023.day-11
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

;; --- Day 11: Cosmic Expansion ---
;; https://adventofcode.com/2023/day/11

(def input (aoc/get-input 2023 11))

(defn empty-space [input]
  (let [lines (str/split-lines input)]
    {:rows (keep-indexed (fn [r l] (when (re-find #"^\.+$" l) r)) lines)
     :cols (keep-indexed (fn [c l] (when (every? #(= % \.) l) c)) (aoc/transpose lines))}))

(defn in-range? [a b c]
  (cond
    (== a b) false
    (< a b)  (< a c b)
    (< b a)  (< b c a)))

(defn solve [input t]
  (let [t (dec t)
        galaxies (keys (g2/parse input #(= % \#)))
        {empty-rows :rows empty-cols :cols} (empty-space input)]
    (->> (comb/combinations galaxies 2)
         (map (fn [[[x1 y1] [x2 y2]]]
                (let [xx (* (count (filter #(in-range? x1 x2 %) empty-cols)) t)
                      yy (* (count (filter #(in-range? y1 y2 %) empty-rows)) t)
                      [x1 x2] (if (< x1 x2) [x1 (+ x2 xx)] [(+ x1 xx) x2])
                      [y1 y2] (if (< y1 y2) [y1 (+ y2 yy)] [(+ y1 yy) y2])]
                  (aoc/manhattan-distance [x1 y1] [x2 y2]))))
         (aoc/sum))))

;; part 1
(solve input 2) ; 9608724

;; part 2
(solve input 1000000) ; 904633799472
