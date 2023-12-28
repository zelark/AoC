(ns zelark.aoc-2023.day-06
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.matgic :as mg]
            [clojure.string :as str]))

;; --- Day 6: Wait For It ---
;; https://adventofcode.com/2023/day/6

(def input (aoc/get-input 2023 06))

(defn parse [input & {:keys [ignore-spaces?]}]
  (cond->> (str/split-lines input)
    ignore-spaces? (map #(str/escape % {\space ""}))
    :always        (map aoc/parse-longs)))

;; t = max-time - speed; dist / (max-time - speed) = speed
;; => -speed^2 + speed*max-time - dist = 0
(defn ways-to-win [max-time dist]
  (->> (mg/quadratic -1 max-time (- dist))
       (map long)
       (apply -)))

(comment
  ;; My best attempt before I knew it could be solved with a quadratic equation.
  ;; 532.88108 msecs for part 2
  (defn ways-to-win [max-time dist]
    (some (fn [speed]
            (when (< dist (* (- max-time speed) speed))
              (inc (- (- max-time speed) speed))))
          (range 1 max-time))))

;; part 1 (0.66555 msecs)
(->> (parse input)
     (apply map ways-to-win)
     (aoc/mul)) ; 741000

;; part 2 (0.543679 msecs)
(->> (parse input {:ignore-spaces? true})
     (apply map ways-to-win)
     (first)) ; 38220708
