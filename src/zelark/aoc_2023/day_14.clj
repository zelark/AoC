(ns zelark.aoc-2023.day-14
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 14: Parabolic Reflector Dish ---
;; https://adventofcode.com/2023/day/14

(def input (aoc/get-input 2023 14))

(defn parse [input]
  (str/split-lines input))

(defn tilt-row [row]
  (loop [x   (first row)
         row (rest row)
         res []
         space []]
    (if-not x
      (str/join (into res space))
      (case x
        \. (recur (first row) (rest row) res (conj space x))
        \O (recur (first row) (rest row) (conj res x) space)
        \# (recur (first row) (rest row) (conj (into res space) x) [])))))

(defn tilt [panel]
  (mapv tilt-row panel))

(defn calc-load [panel]
  (let [len (count panel)]
    (->> (map-indexed (fn [i row]
                        (reduce #(if (= %2 \O) (+ %1 (- len i)) %1) 0 row))
                      panel)
         (aoc/sum))))

;; part 1
(let [panel (parse input)]
  (->> (aoc/transpose panel)
       (tilt)
       (aoc/transpose)
       (calc-load))) ; 105623

;; part 2
(defn spin-cycle [tile]
  (-> tile tilt aoc/rotate tilt aoc/rotate tilt aoc/rotate tilt aoc/rotate))

(let [panel (parse input)
      anti-rotate (comp aoc/rotate aoc/rotate aoc/rotate)
      [i j seen] (->> (anti-rotate panel)
                      (iterate spin-cycle)
                      (map-indexed vector)
                      (reduce (fn [seen [idx p]] (if (seen p) (reduced [(seen p) idx seen]) (assoc seen p idx))) {}))
      n (mod (- 1000000000 (dec i))
             (- j i))]
  (-> ((set/map-invert seen) (+ (dec i) n))
      (rotate)
      (calc-load))) ; 98029

;; "Elapsed time: 3754.234856 msecs"
