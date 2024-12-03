(ns zelark.aoc-2024.day-02
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 2: Red-Nosed Reports ---
;; https://adventofcode.com/2024/day/2

(def input (aoc/get-input 2024 2))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map aoc/parse-longs)))

(defn safe? [levels]
  (let [diffs (->> (partition 2 1 levels)
                   (map (fn [[a b]] (- a b))))]
    (or (every? #{+1 +2 +3} diffs)
        (every? #{-1 -2 -3} diffs))))

;; part 1 (5.941968 msecs)
(->> (parse-input input)
     (filter safe?)
     (count)) ; 236

;; part 2 (20.661422 msecs)
(defn tsafe? [levels]
  (or (safe? levels)
      (->> (range (count levels))
           (map #(aoc/remove-nth levels %))
           (some safe?))))

(->> (parse-input input)
     (filter tsafe?)
     (count)) ; 308
