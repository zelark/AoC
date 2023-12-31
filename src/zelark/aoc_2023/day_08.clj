(ns zelark.aoc-2023.day-08
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.matgic :as mg]
            [clojure.string :as str]))

;; --- Day 8: Haunted Wasteland ---
;; https://adventofcode.com/2023/day/8

(def input (aoc/get-input 2023 8))

(defn parse-nodes [nodes]
  (->> (re-seq #"([A-Z]{3}) = \(([A-Z]{3}), ([A-Z]{3})\)" nodes)
       (reduce (fn [m [_ node left right]] (assoc m node [left right])) {})))

(defn parse-input [input] 
  (let [[instructions nodes] (aoc/split-on-blankline input)]
    [instructions (parse-nodes nodes)]))

(defn next-node [nodes node instruction]
  (case instruction
    \L (nth (get nodes node) 0)
    \R (nth (get nodes node) 1)))

(defn steps [nodes instructions start end?]
  (reduce (fn [[n node] ins]
            (if (end? node)
              (reduced n)
              [(inc n) (next-node nodes node ins)]))
          [0 start]
          (cycle instructions)))

;; part 1 (14.949374 msecs)
(let [[instructions nodes] (parse-input input)]
  (steps nodes instructions "AAA" #{"ZZZ"})) ; 18673

;; part 2 (24.9524 msecs)
(let [[instructions nodes] (parse-input input)
      starts (->> (keys nodes)
                  (filter #(str/ends-with? % "A")))
      end? #(str/ends-with? % "Z")]
  (->> (map #(steps nodes instructions % end?) starts)
       (apply mg/lcm))) ; 17972669116327
