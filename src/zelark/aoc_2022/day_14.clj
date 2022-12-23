(ns zelark.aoc-2022.day-14
  (:require [clojure.string :as str]
            [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 14: Regolith Reservoir ---
;; https://adventofcode.com/2022/day/14

(def input (aoc/get-input 2022 14))

(defn make-line [xs]
  (set (mapcat g2/straight-line-points xs (rest xs))))

(defn parse [input]
  (let [points (->> (str/split-lines input)
                    (map #(partition 2 (aoc/parse-longs %)))
                    (map make-line)
                    (reduce into))]
    (zipmap points (repeat \#))))

(defn next-loc [state max-y loc]
  (first (for [delta [[0 +1] [-1 +1] [+1 +1]]
               :let  [[x y] (mapv + loc delta)]
               :when (and (not (state [x y]))
                          (< y (+ max-y 2)))]
           [x y])))

(defn step [state max-y loc]
  (let [[x y] (->> (iterate #(next-loc state max-y %) loc)
                   (take-while some?)
                   (last))]
    (if (<= max-y y)
      state
      (assoc state [x y] \o))))

(def start-point
  "The sand is pouring into the cave from point 500,0."
  [500 0])

;; part 1
(let [cave (parse input)
      [_ [_ max-y]]  (g2/boundaries cave)
      cave-with-sand (aoc/fix-point #(step % max-y start-point) cave)]
  (g2/count cave-with-sand \o)) ; => 614

;; part 2
;; TODO: merge step and step-2 in one function.
(defn step-2 [state max-y loc]
  (let [loc' (->> (iterate #(next-loc state max-y %) loc)
                  (take-while some?)
                  (last))]
    (if-not loc'
      state
      (assoc state loc' \o))))

(let [cave (parse input)
      [_ [_ max-y]] (g2/boundaries cave)
      cave-with-sand (aoc/fix-point #(step-2 % max-y start-point) cave)]
  (g2/count cave-with-sand \o)) ; => 26170
