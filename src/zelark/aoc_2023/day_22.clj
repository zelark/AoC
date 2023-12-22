(ns zelark.aoc-2023.day-22
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 22: Sand Slabs ---
;; https://adventofcode.com/2023/day/22

(def input (aoc/get-input 2023 22))

(defn parse-line [line]
  (->> line (aoc/parse-longs) (split-at 3) (mapv vec)))

(defn parse [input]
  (->> (str/split-lines input)
       (mapv parse-line)))

(def x1 (comp first first))
(def x2 (comp first second))

(def y1 (comp second first))
(def y2 (comp second second))

(def z1 (comp peek first))
(def z2 (comp peek second))

(defn move-down [brick]
  (-> brick
      (update-in [0 2] dec)
      (update-in [1 2] dec)))

(defn brick-cubes [brick]
  (for [x (range (x1 brick) (inc (x2 brick)))
        y (range (y1 brick) (inc (y2 brick)))
        z (range (z1 brick) (inc (z2 brick)))]
    [x y z]))

(defn settle-one [collision? brick]
  (let [brick' (move-down brick)]
    (if (or (zero? (z1 brick')) ; The ground is at z=0 and is perfectly flat.
            (some collision? (brick-cubes brick')))
      brick
      brick')))

(defn settle-all [bricks]
  (loop [stack  []
         bricks (sort-by z1 bricks)
         cubes #{}
         fallen 0]
    (if (seq bricks)
      (let [brick  (first bricks)
            brick' (aoc/fix-point (partial settle-one cubes) brick)]
        (recur (conj stack brick')
               (next bricks)
               (into cubes (brick-cubes brick'))
               (if (= brick brick') fallen (inc fallen))))
      {:bricks (set stack), :fallen fallen})))

(defn solve [input]
  (let [bricks  (parse input)
        settled (:bricks (settle-all bricks))]
    (reduce (fn [res brick]
              (let [bricks (disj settled brick)
                    fallen (:fallen (settle-all bricks))]
                (cond-> res
                  (zero? fallen) (update :part-1 inc)
                  (pos? fallen) (update :part-2 + fallen))))
            {:part-1 0 :part-2 0}
            settled)))

(def answer (solve input))

;; part 2
(:part-1 answer) ; 519 bricks could be safely chosen as the one to get disintegrated.

;; part 1
(:part-2 answer) ; 109531 bricks that would fall.

