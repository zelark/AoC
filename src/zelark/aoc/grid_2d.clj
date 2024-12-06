(ns zelark.aoc.grid-2d
  (:refer-clojure :exclude [count])
  (:require [clojure.string :as str]
            [zelark.aoc.core :as aoc]))

;; General things
(def right [+1  0])
(def left  [-1  0])
(def up    [0  -1])
(def down  [0  +1])

(def empty-space \.)
(def empty-space? #{empty-space})
(def something? (complement empty-space?))
(defn any-but [ch] (complement #{ch}))

(defn parse 
  ([input] (parse input identity identity))
  ([input pred] (parse input pred identity))
  ([input pred post]
   (->> (str/split-lines input)
        (mapcat (fn [y line] (keep-indexed (fn [x ch] (when (pred ch) [[x y] ch])) line)) (range))
        (reduce (fn [m [loc ch]] (assoc m loc (post ch))) {}))))

(defn plus [[^long x ^long y] [^long dx ^long dy]]
  [(+ x dx) (+ y dy)])

(defn minus [[^long x ^long y] [^long dx ^long dy]]
  [(- x dx) (- y dy)])

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

(defn center [grid]
  (->> (boundaries grid)
       (second)
       (mapv #(quot % 2))))

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

(defn straight-line-points [[x1 y1 :as a] [x2 y2 :as b]]
  (when (straight-line? a b)
    (for [x (aoc/rangex x1 x2)
          y (aoc/rangex y1 y2)]
      [x y])))

(defn diagonal-line-points [[x1 y1] [x2 y2]]
  (map vector (aoc/rangex x1 x2) (aoc/rangex y1 y2)))

(defn line-points [a b]
  (if (straight-line? a b)
    (straight-line-points a b)
    (diagonal-line-points a b)))

(defn close-points
  "Closes a seq of points if it is not.

  ([0 0] [0 5] [5 5]) -> [[0 0] [0 5] [5 5] [0 0]]
  
  Returns a vector of the points."
  [points]
  (let [points (vec points)
        [start end] ((juxt first peek) points)]
    (cond-> points
      (not= start end) (conj start))))

(defn perimeter-of-polygon [points]
  (let [points (close-points points)]
    (reduce + (map aoc/manhattan-distance points (rest points)))))

;; https://en.wikipedia.org/wiki/Shoelace_formula#Triangle_formula
(defn area-of-polygon [points]
  (let [sum-diagonals (fn [acc [[x1 y1] [x2 y2]]]
                        (+ acc (- (* x1 y2) (* x2 y1))))]
    (abs (/ (reduce sum-diagonals 0 (partition 2 1 points)) 2))))

;; Rotation
;; https://en.wikipedia.org/wiki/Rotation_matrix#Common_2D_rotations
(def rotation-matrix {90  [0  1 -1  0]
                      180 [-1  0  0 -1]
                      270 [0 -1  1  0]})

(defn rotate-ccw [[^long dx ^long dy] angle]
  (let [[a b c d] (rotation-matrix angle)]
    [(+ (* dx a) (* dy b))
     (+ (* dx c) (* dy d))]))

(defn turn [direction xy]
  (case direction
    :right (rotate-ccw xy 270)
    :left  (rotate-ccw xy 90)
    :back  (rotate-ccw xy 180)))
