(ns zelark.aoc-2023.day-02
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 2: Cube Conundrum ---
;; https://adventofcode.com/2023/day/2

(def input (aoc/get-input 2023 02))

(defn parse-subsets [subsets]
  (->> (str/split subsets #";")
       (map #(re-seq #"(\d+)\s(red|green|blue)" %))
       (mapv (fn [subset]
               (reduce (fn [m [_ n color]]
                         (assoc m (keyword color) (parse-long n)))
                       {}
                       subset)))))

(defn parse-line [line]
  (let [[_ game subsets] (re-find #"Game (\d+): (.+)" line)]
    {:game    (parse-long game)
     :subsets (parse-subsets subsets)}))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn possible?
  "Only 12 red cubes, 13 green cubes, and 14 blue cubes."
  [{:keys [red green blue]
    :or {red 0 green 0 blue 0}}]
  (and (<= red   12)
       (<= green 13)
       (<= blue  14)))

;; part 1
(->> (parse-input input)
     (filter (fn [{:keys [subsets]}] (every? possible? subsets)))
     (reduce (fn [acc {:keys [game]}] (+ acc game)) 0)) ; 2076

;; part 2
(->> (parse-input input)
     (map (fn [{:keys [subsets]}]
            (reduce #(merge-with max %1 %2) subsets)))
     (reduce (fn [acc {:keys [red green blue]}]
               (+ acc (* red green blue))) 0)) ; 70950
