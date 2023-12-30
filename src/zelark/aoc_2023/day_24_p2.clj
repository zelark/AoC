(ns zelark.aoc-2023.day-24-p2
  (:require [clojure.math.combinatorics :as comb]
            [zelark.aoc-2023.day-24 :refer [parse input]]
            [zelark.aoc.core :as aoc]
            [zelark.aoc.matgic :as mg]))

;; --- Day 24 (Part 2): Never Tell Me The Odds ---
;; https://adventofcode.com/2023/day/24

;; Thanks @wevrem for the idea:
;; https://github.com/wevre/advent-of-code/blob/master/src/advent_of_code/2023/day_24.clj

;; Also an interesting comment:
;; https://www.reddit.com/r/adventofcode/comments/18pnycy/comment/keq6s6s/

(defn common-elements
  "Returns common elements of l1 and l2 (duplicates included).
  Assumes l1 and l2 sorted asc."
  [l1 l2]
  (loop [[n & nn :as l1] l1
         [m & mm :as l2] l2
         acc []]
    (if (and n m)
      (cond
        (< n m) (recur nn l2 acc)
        (< m n) (recur l1 mm acc)
        :else   (recur nn mm (conj acc n)))
      acc)))

(defn pos-diffs [group axis]
  (let [positions (map #(-> % :pos axis) group)]
    (->> (comb/combinations positions 2)
         (map (fn [[^long a ^long b]] (abs (- a b)))))))

(defn probable-velocities [[v group] axis]
  (let [common-factors (->> (pos-diffs group axis)
                            (map mg/factors)
                            (reduce common-elements))]
    (if (seq common-factors)
      (->> (comb/subsets common-factors)
           (rest) ; Skip empty subset ().
           (map aoc/mul)
           (mapcat (fn [u] [(- v u) (+ v u)])))
      [(dec v) (inc v)]))) ; There is a prime diff.

(defn valid-candidate? [candidate diffs]
  (or (zero? candidate) ; Velocity of the group equals the test velocity.
      (every? #(zero? (mod % candidate)) diffs)))

(defn find-rock-velocity [axis hailstones]
  (let [v+group (->> (group-by #(-> % :vel axis) hailstones)
                     (filter (fn [[_ group]] (aoc/cnt group >= 2))))]
    (-> (reduce-kv (fn [vs v group]
                     (let [diffs (pos-diffs group axis)]
                       (filter #(valid-candidate? (- % v) diffs) vs)))
                   (probable-velocities (first v+group) axis)
                   (rest v+group))
        (first))))

;; xi = u1*t1 + x1 = ur*t1 + xr
;; yi = v1*t1 + y1 = vr*t1 + yr
;; xj = u2*t2 + x2 = ur*t2 + xr
;; yj = v2*t2 + y2 = vr*t2 + yr
;; After a few transforamtion we will get an quationt for t1 (below).

;; (61.796851 msecs)
(let [hailstones (parse input)
      [vx vy vz] (pmap #(find-rock-velocity % hailstones) [:x :y :z])
      [h1 h2]    hailstones
      {px1 :x py1 :y pz1 :z} (h1 :pos)
      {vx1 :x vy1 :y vz1 :z} (h1 :vel)
      {px2 :x py2 :y} (h2 :pos)
      {vx2 :x vy2 :y} (h2 :vel)
      t1 (/ (- (* (- px1 px2) (- vy2 vy)) (* (- py1 py2) (- vx2 vx)))
            (- (* (- vy1 vy)  (- vx2 vx)) (* (- vx1 vx)  (- vy2 vy))))
      px (+ px1 (* t1 (- vx1 vx)))
      py (+ py1 (* t1 (- vy1 vy)))
      pz (+ pz1 (* t1 (- vz1 vz)))]
  (+ px py pz)) ; 849377770236905


