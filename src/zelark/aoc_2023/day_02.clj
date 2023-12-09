(ns zelark.aoc-2023.day-02
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 2: Cube Conundrum ---
;; https://adventofcode.com/2023/day/2

(def input (aoc/get-input 2023 02))

(defn parse-line [line]
  (let [[game line] (str/split line #":")
        sets        (str/split line #";")]
    {:game (parse-long (re-find #"\d+" game))
     :sets
     (->> (map #(re-seq #"(\d+)\s([a-z]+)" %) sets)
          (mapv (fn [set]
                  (reduce (fn [m [_ n color]]
                            (assoc m (keyword color) (parse-long n)))
                          {}
                          set))))}))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)))

;; part 1
(defn possible?
  "only 12 red cubes, 13 green cubes, and 14 blue cubes"
  [{:keys [red green blue]
    :or {red 0 green 0 blue 0}}]
  (and (<= red   12)
       (<= green 13)
       (<= blue  14)))

(->> (parse-input input)
     (filter (fn [{:keys [sets]}] (every? possible? sets)))
     (reduce (fn [acc {:keys [game]}] (+ acc game)) 0)) ; 2076

;; part 2
(->> (parse-input input)
     (map (fn [{:keys [sets]}]
            (reduce (fn [[r g b] {:keys [red green blue]
                                  :or {red 0 green 0 blue 0}}]
                      [(max r red)
                       (max g green)
                       (max b blue)])
                    [0 0 0]
                    sets)))
     (reduce (fn [acc [r g b]]
               (+ acc (* r g b))) 0)) ; 70950
