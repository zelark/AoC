(ns zelark.aoc-2023.day-xx
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [medley.core :as mdl]))

;; 
;; https://adventofcode.com/2023/day/xx

(def input (aoc/get-input 2023 xx))

(defn parse [input]
  (->> (str/split-lines input)
       (mapv aoc/parse-longs)))

(def s1 "")

(parse s1)

;; part 1


;; part 2

