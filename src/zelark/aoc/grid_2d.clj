(ns zelark.aoc.grid-2d
  (:require [clojure.string :as str]))

(defn parse [input pred]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (keep-indexed (fn [x ch] (when (pred ch) [[x y] ch])) line)) (range))
       (reduce (fn [m [loc ch]] (assoc m loc ch)) {})))

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
  (let [min-x (apply min (map #(nth % 0) points))
        max-x (apply max (map #(nth % 0) points))
        min-y (apply min (map #(nth % 1) points))
        max-y (apply max (map #(nth % 1) points))]
    [[min-x min-y]
     [max-x max-y]]))

(defn extend-boundaries [[lower upper]]
  [(mapv dec lower)
   (mapv inc upper)])

(defn in-bounds? [boundaries [x y]]
  (let [[[xl yl] [xr yr]] boundaries]
    (and (<= xl x xr)
         (<= yl y yr))))
