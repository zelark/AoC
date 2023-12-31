(ns zelark.aoc-2023.day-04
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 4: Scratchcards ---
;; https://adventofcode.com/2023/day/4

(def input (aoc/get-input 2023 04))

(defn parse-line [line]
  (let [[card nubmers] (str/split line #":")
        [n1 n2]        (str/split nubmers #"\|")]
    [(parse-long (re-find #"\d+" card))
     (count (set/intersection (set (aoc/parse-longs n1))
                              (set (aoc/parse-longs n2))))]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)))

;; part 1 (6.389213 msecs)
(->> (parse-input input)
     (aoc/sum (fn [[_ w]] (if (zero? w) w (bit-shift-left 1 (dec w)))))) ; 23028

;; part 2 (7.019881 msecs)
(let [cards (parse-input input)]
  (->> cards
       (reduce (fn [m [n w]] ; Add copies of winning cards.
                 (merge-with + m (zipmap (range (inc n) (inc (+ n w)))
                                         (repeat (m n 1)))))
               (zipmap (map first cards) (repeat 1)))
       (aoc/sum val))) ; 9236992
