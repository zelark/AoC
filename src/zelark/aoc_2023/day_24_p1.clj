(ns zelark.aoc-2023.day-24-p1
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]
            [zelark.aoc.core :as aoc]
            [zelark.aoc.range :as r]))

;; --- Day 24 (Part 1): Never Tell Me The Odds ---
;; https://adventofcode.com/2023/day/24

(def input (aoc/get-input 2023 24))

(defn ->hailstone [line]
  (let [[px py pz vx vy vz] (aoc/parse-longs line)
        pos (zipmap [:x :y :z] [px py pz])
        vel (zipmap [:x :y :z] [vx vy vz])]
    {:pos pos :vel vel}))

(defn parse [input]
  (mapv ->hailstone (str/split-lines input)))

(defn intersection [h1 h2]
  (let [[k1 b1] (h1 :lineq)
        [k2 b2] (h2 :lineq)]
    (when (not= k1 k2)
      (let [x (/ (- b1 b2) (- k2 k1))
            y (+ (* k1 x) b1)]
        [x y]))))

(defn in-future? [h [x2 y2]]
  (let [{x1 :x y1 :y} (h :pos)
        {vx :x vy :y} (h :vel)]
    (and (or (and (pos? vx) (< x1 x2))
             (and (neg? vx) (> x1 x2)))
         (or (and (pos? vy) (< y1 y2))
             (and (neg? vy) (> y1 y2))))))

(defn will-intersect? [test-area h1 h2]
  (when-let [[cx cy] (intersection h1 h2)]
    (and (r/contains? test-area cx)
         (r/contains? test-area cy)
         (in-future? h1 [cx cy])
         (in-future? h2 [cx cy]))))

(defn +lineq [hailstone]
  (let [{x1 :x y1 :y} (hailstone :pos)
        x2 (+ x1 (-> hailstone :vel :x))
        y2 (+ y1 (-> hailstone :vel :y))
        k (/ (- y2 y1) (- x2 x1))
        b (- y1 (* k x1))]
    (assoc hailstone :lineq [k b])))

;; (170.060927 msecs)
(let [hailstones (map +lineq (parse input))
      test-area  (r/rangel 200000000000000 200000000000001)]
  (->> (comb/combinations hailstones 2)
       (filter #(will-intersect? test-area (first %) (second %)))
       (count))) ; 19976
