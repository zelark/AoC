(ns zelark.aoc-2024.day-01
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 1: Historian Hysteria ---
;; https://adventofcode.com/2024/day/1

(def input (aoc/get-input 2024 1))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map aoc/parse-longs)
       (aoc/transpose)))

;; part 1 (2.193505 msecs)
(->> (parse-input input)
     (map sort)
     (apply map (fn [a b] (abs (- a b))))
     (aoc/sum)) ; 1197984

;; part 2 (3.470023 msecs)
(let [[l1 l2] (parse-input input)
      fs      (frequencies l2)]
  (aoc/sum #(* % (get fs % 0)) l1)) ; 23387399
