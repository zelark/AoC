(ns zelark.aoc-2024.day-11
  (:require [zelark.aoc.core :as aoc]
            [clojure.math :as math]))

;; --- Day 11: Plutonian Pebbles ---
;; https://adventofcode.com/2024/day/11

(def input (aoc/get-input 2024 11))

(defn parse-input [input]
  (aoc/parse-longs input))

(defn split-stone [stone]
  (let [length  (aoc/nlen stone)
        half    (quot length 2)
        divisor (int (math/pow 10 half))]
    [(quot stone divisor)
     (mod stone divisor)]))

(defn blink [stones]
  (let [add-stones #(update %1 %2 (fnil + 0) %3)]
    (reduce-kv (fn [m stone num]
                 (cond
                   (zero? stone)
                   (add-stones m 1 num)

                   (even? (aoc/nlen stone))
                   (reduce #(add-stones %1 %2 num) m (split-stone stone))

                   :else
                   (add-stones m (* stone 2024) num)))
               {}
               stones)))

(defn solve [input n]
  (->> (parse-input input)
       (frequencies)
       (iterate blink)
       (drop n)
       (first)
       (aoc/sum val)))

;; part 1 (2.303339 msecs)
(solve input 25) ; 203228

;; part 2 (70.944065 msecs)
(solve input 75) ; 240884656550923
