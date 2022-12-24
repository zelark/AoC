(ns zelark.aoc.grid-2d
  (:refer-clojure :exclude [count])
  (:require [clojure.string :as str]
            [zelark.aoc.core :as aoc]))

;; General things

(defn parse [input pred]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (keep-indexed (fn [x ch] (when (pred ch) [[x y] ch])) line)) (range))
       (reduce (fn [m [loc ch]] (assoc m loc ch)) {})))

(defn plus [[^long x ^long y] [^long dx ^long dy]]
  [(+ x dx) (+ y dy)])

(defn neighbors [[x y]]
  (for [[dx dy] [[-1  0] [1 0]
                 [ 0 -1] [0 1]]]
    [(+ x dx) (+ y dy)]))

(defn all-neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not= 0 dx dy)]
    [(+ x dx) (+ y dy)]))

(defn boundaries [points]
  (let [points (cond-> points (map? points) keys)
        [min-x max-x] (apply (juxt min max) (map first  points))
        [min-y max-y] (apply (juxt min max) (map second points))]
    [[min-x min-y]
     [max-x max-y]]))

(defn extend-boundaries [[lower upper]]
  [(mapv dec lower)
   (mapv inc upper)])

(defn narrow-boundaries [[lower upper]]
  [(mapv inc lower)
   (mapv dec upper)])

(defn in-bounds? [boundaries [x y]]
  (let [[[xl yl] [xr yr]] boundaries]
    (and (<= xl x xr)
         (<= yl y yr))))

(defn count
  "Counts occurrences of `v` among `grid` values."
  [grid v]
  (transduce (keep #(when (= % v) 1)) + (vals grid)))

;; Lines

(defn straight-line? [[x1 y1] [x2 y2]]
  (or (== x1 x2) (== y1 y2)))

(defn straight-line-points [[x1 y1] [x2 y2]]
  (for [x (aoc/rangex x1 x2)
        y (aoc/rangex y1 y2)]
    [x y]))

(defn diagonal-line-points [[x1 y1] [x2 y2]]
  (map vector (aoc/rangex x1 x2) (aoc/rangex y1 y2)))

(defn line-points [a b]
  (if (straight-line? a b)
    (straight-line-points a b)
    (diagonal-line-points a b)))
