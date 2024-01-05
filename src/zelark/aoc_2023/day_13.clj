(ns zelark.aoc-2023.day-13
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 13: Point of Incidence ---
;; https://adventofcode.com/2023/day/13

(def input (aoc/get-input 2023 13))

(defn parse [input]
  (->> (aoc/split-on-blankline input)
       (map #(str/split-lines %))))

;; 1 #...##..# 1
;; 2 #....#..# 2
;; 3 ..##..### 3
;; 4v#####.##.v4
;; 5^#####.##.^5
;; 6 ..##..### 6
;; 7 #....#..# 7

;; This pattern reflects across the horizontal line between rows 4 and 5.
;; Row 1 would reflect with a hypothetical row 8, but since that's not in the
;; pattern, row 1 doesn't need to match anything. The remaining rows match:
;; row 2 matches row 7, row 3 matches row 6, and row 4 matches row 5.

(defn horizontal-line [mirror? pattern]
  (loop [lines (rest pattern)
         state (take 1 pattern)]
    (when (seq lines)
      (if (mirror? lines state)
        (count state)
        (recur (rest lines)
               (cons (first lines) state))))))

(defn vertical-line [mirror? pattern]
  (horizontal-line mirror? (aoc/transpose pattern)))

(defn mirror? [a b]
  (= (take (count a) b)
     (take (count b) a)))

(defn mirror-w-smudge? [a b]
  (let [a (str/join (take (count b) a))
        b (str/join (take (count a) b))]
    (aoc/one? (reduce + (map #(if (= %1 %2) 0 1) a b)))))

(defn solve [mirror? input]
  (->> (parse input)
       (reduce (fn [acc pattern]
                 (+ acc (or (vertical-line mirror? pattern)
                            (* 100 (horizontal-line mirror? pattern))))) 0)))

;; part 1 (5.411 msecs)
(time (solve mirror? input)) ; 27300

;; part 2 (16.287 msecs)
(time (solve mirror-w-smudge? input)) ; 29276
