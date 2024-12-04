(ns zelark.aoc-2024.day-04
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 4: Ceres Search ---
;; https://adventofcode.com/2024/day/4

(def input (aoc/get-input 2024 4))

(defn parse-input [input]
  (g2/parse input))

(defn xmas-locs [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [[(+ x dx)       (+ y dy)]
     [(+ x dx dx)    (+ y dy dy)]
     [(+ x dx dx dx) (+ y dy dy dy)]]))

(defn letters [grid locs]
  (map grid locs))

;; part 1 (91.405143 msecs)
(let [grid (parse-input input)]
  (reduce-kv (fn [acc loc ch]
               (if (= ch \X)
                 (-> (map #(letters grid %) (xmas-locs loc))
                     (aoc/cnt [\M \A \S])
                     (+ acc))
                 acc))
             0
             grid)) ; 2427

;; part 2 (81.531023 msecs)
(defn mas-locs [[x y]]
  (for [dx [-1 1]
        dy [-1 1]]
    [[(+ x dx)    (+ y dy)]
     [(+ x dx dx) (+ y dy dy)]]))

(let [grid (parse-input input)]
  (-> (reduce-kv (fn [acc loc ch]
                   (if (= ch \M)
                     (->> (mas-locs loc)
                          (keep #(when (= (letters grid %) [\A \S]) (first %)))
                          (into acc))
                     acc))
                 []
                 grid)
      (frequencies)
      (aoc/cnt 2))) ; 1900
