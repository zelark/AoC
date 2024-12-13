(ns zelark.aoc-2024.day-13
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 13: Claw Contraption ---
;; https://adventofcode.com/2024/day/13

(def input (aoc/get-input 2024 13))

(defn parse-machine [input part2?]
  (let [[ax ay bx by px py] (aoc/parse-longs input)]
    {:a [ax ay]
     :b [bx by]
     :p (cond->> [px py] part2? (mapv + [10000000000000 10000000000000]))}))

(defn parse-input [input part2?]
  (->> (aoc/split-on-blankline input)
       (map #(parse-machine % part2?))))

(defn win-prize [machine]
  (let [[ax ay] (get machine :a)
        [bx by] (get machine :b)
        [px py] (get machine :p)
        n (/ (- (* py bx) (* px by))
             (- (* ay bx) (* ax by)))
        m (/ (- px (* n ax)) bx)]
    [n m]))

(defn solve [part input]
  (->> (parse-input input (= part :p2))
       (map win-prize)
       (filter (comp (complement ratio?) first))
       (aoc/sum (fn [[a b]] (+ (* 3 a) b)))))

;; part 1 (2.268923 msecs)
(solve :p1 input) ; 28262

;; part 2 (3.146691 msecs)
(solve :p2 input) ; 101406661266314
