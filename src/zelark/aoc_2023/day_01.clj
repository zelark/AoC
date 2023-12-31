(ns zelark.aoc-2023.day-01
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 1: Trebuchet?! ---
;; https://adventofcode.com/2023/day/1

(def input (aoc/get-input 2023 1))

(defn parse-input [input]
  (str/split-lines input))

(def text-digits ["zero" "one" "two"   "three" "four"
                  "five" "six" "seven" "eight" "nine"])

(defn find-digits [pattern]
  (fn [s]
    (->> (re-seq pattern s)
         (mapv (fn [[_ x]]
                 (if-let [n (parse-long x)]
                   n
                   (.indexOf text-digits x)))))))

(defn solve [find-digits input]
  (->> (parse-input input)
       (map find-digits)
       (map (fn [x]
              (let [[a b] ((juxt first peek) x)]
                (+ (* a 10) b))))
       (aoc/sum)))

;; part 1 (3.031089 msecs)
(let [pattern #"(\d)"]
  (solve (find-digits pattern) input)) ; 54239

;; part 2 (32.813569 msecs)
(let [pattern #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))"]
  (solve (find-digits pattern) input)) ; 55343
