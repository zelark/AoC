(ns zelark.aoc-2022.day-13
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 13: Distress Signal ---
;; https://adventofcode.com/2022/day/13

(def input (aoc/get-input 2022 13))

(defn parse-pair [p]
  (->> (str/split-lines p)
       (mapv edn/read-string)))

(defn parse-input [input]
  (->> (aoc/split-on-blankline input)
       (map parse-pair)))

(defn packet-cmp [a b]
  (cond
    (and (number? a) (number? b))
    (- a b)

    (and (vector? a) (vector? b))
    (or (->> (map packet-cmp a b)
             (drop-while zero?)
             (first))
        (- (count a) (count b)))
    
    (number? a) (recur [a] b)
    (number? b) (recur a [b])
    :else       (throw (IllegalArgumentException. "Wrong packet was passed."))))

(comment
  ;; Default comparing for vectors is almost what we need
  ;; but in reverse order: first compare for elements,
  ;; and only then compare for sizes.
  (compare    [0 0 0 0] [1 1 1])  ; => 1
  (packet-cmp [0 0 0 0] [1 1 1])  ; => -1
)

(defn right-order? [a b]
  (neg? (packet-cmp a b)))

;; part 1
(->> (parse-input input)
     (keep-indexed (fn [idx [a b]] (when (right-order? a b) (inc idx))))
     (aoc/sum)) ; 5350

;; part 2
(let [packets (mapcat identity (parse-input input))
      d2 [[2]]
      d6 [[6]]
      delimiter? #{d2 d6}]
  (->> (into packets [d2 d6])
       (sort packet-cmp)
       (keep-indexed (fn [idx a] (when (delimiter? a) (inc idx))))
       (aoc/mul))) ; 19570
