(ns zelark.aoc-2022.day-15
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 15: Beacon Exclusion Zone ---
;; https://adventofcode.com/2022/day/15

(def input (aoc/get-input 2022 15))

(defn parse [input]
  (->> (str/split-lines input)
       (reduce (fn [m line]
                 (let [[sx sy bx by] (aoc/parse-longs line)
                       dist (aoc/manhattan-distance [sx sy] [bx by])]
                   (-> m
                       (update :beacons conj  {:x bx, :y by})
                       (update :scanners conj {:x sx, :y sy, :dist dist}))))
               {:beacons #{}, :scanners #{}})))

;; part 1
(let [{:keys [scanners beacons]} (parse input)
      ry 2000000
      [x1 x2] (reduce (fn [[l r :as l+r] {:keys [x y dist]}]
                        (let [gap (abs (- y ry))]
                          (if (<= gap dist)
                            [(min l (- x (- dist gap)))
                             (max r (+ x (- dist gap)))]
                            l+r)))
                      [0 0]
                      scanners)]
  (- (inc (- x2 x1))
     (count (filter #(= (:y %) ry) beacons)))) ; => 5809294
