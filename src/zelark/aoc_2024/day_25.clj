(ns zelark.aoc-2024.day-25
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 25: Code Chronicle ---
;; https://adventofcode.com/2024/day/25

(def input (aoc/get-input 2024 25))

(defn heights [item]
  (->> (aoc/transpose item)
       (mapv #(dec (aoc/cnt % \#)))))

(defn parse-input [input]
  (let [{locks "#####" keys "....."}
        (->> (aoc/split-on-blankline input :split-lines? true)
             (group-by first))]
    {:locks (map heights locks)
     :keys  (map heights keys)}))

(defn fit? [lock key]
  (->> (map + lock key)
       (every? #(<= 0 % 5))))

;; part 1 (45.819708 msecs)
(let [{:keys [keys locks]} (parse-input input)]
  (-> (for [lock locks, key keys :when (fit? lock key)] 1)
      (aoc/sum))) ; 3133
