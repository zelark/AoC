(ns zelark.aoc-2024.day-11
  (:require [zelark.aoc.core :as aoc]
            [clojure.math :as math]))

;; --- Day 11: Plutonian Pebbles ---
;; https://adventofcode.com/2024/day/11

(def input (aoc/get-input 2024 11))

(defn parse-input [input]
  (aoc/parse-longs input))

(defn len [n]
  (inc (int (math/log10 n))))

(defn split-stone [stone]
  (let [length  (len stone)
        half    (quot length 2)
        divisor (int (math/pow 10 half))]
    [(quot stone divisor)
     (mod stone divisor)]))

(defn blink [stones]
  (reduce-kv (fn [m stone n]
               (cond
                 (zero? stone)
                 (update m 1 (fnil + 0) n)

                 (even? (len stone))
                 (let [[st1 st2] (split-stone stone)]
                   (-> m
                       (update st1 (fnil + 0) n)
                       (update st2 (fnil + 0) n)))

                 :else
                 (update m (* stone 2024) (fnil + 0) n)))
             {}
             stones))

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
