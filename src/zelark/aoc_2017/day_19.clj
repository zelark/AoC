(ns zelark.aoc-2017.day-19
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 19: A Series of Tubes ---
;; https://adventofcode.com/2017/day/19

(def input (aoc/get-input 2017 19))

(defn new-dir [maze [x y] [dx dy]]
  (let [dirs [[dy dx] [(- dy) (- dx)]]]
    (some (fn [[dx dy]]
            (let [ch (get-in maze [(+ y dy) (+ x dx)])]
              (when-not (or (nil? ch)
                            (= ch \space))
                [dx dy])))
          dirs)))

(defn move [maze {:keys [pos dir steps letters] :as state}]
  (let [[x y] (mapv + pos dir)
        ch    (get-in maze [y x])]
    (case ch
      (nil \space) state
      \+           (assoc state
                          :pos [x y]
                          :dir (new-dir maze [x y] dir)
                          :steps (inc steps))
      (assoc state
             :pos [x y]
             :letters (cond-> letters (Character/isLetter ch) (str ch))
             :steps (inc steps)))))

(defn solve [input]
  (let [maze (str/split-lines input)
        init-state {:pos [(str/index-of (first maze) \|) 0]
                    :dir [0 1]
                    :steps 1}]
    (aoc/fix-point (partial move maze) init-state)))

;; part 1
(:letters (solve input)) ; GINOWKYXH

;; part 2
(:steps (solve input)) ; 16636
