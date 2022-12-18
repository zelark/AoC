(ns zelark.aoc-2022.day-18
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-3d :as g3]
            [clojure.string :as str]))

;; --- Day 18: Boiling Boulders ---
;; https://adventofcode.com/2022/day/18

(def input (aoc/get-input 2022 18))

(defn parse [input]
  (->> (str/split-lines input)
       (map #(aoc/parse-longs %))
       (set)))

;; part 1
(let [droplets (set (parse input))]
  (->> droplets
       (mapcat g3/neighbors)
       (remove droplets)
       (count))) ; 3550

;; part 2
(defn flood [discover start]
  (loop [seen  #{}
         queue (aoc/queue start)]
    (if-let [current (peek queue)]
      (let [[seen queue] (reduce (fn [[seen queue :as acc] node]
                                   (if (seen node)
                                     acc
                                     [(conj seen node) (conj queue node)]))
                                 [seen (pop queue)]
                                 (discover current))]
        (recur seen queue))
      seen)))

(defn discover [inside? droplets loc]
  (->> (g3/neighbors loc)
       (remove droplets)
       (filter inside?)))

(let [droplets   (parse input)
      boundaries (-> (g3/boundaries droplets)
                     (g3/extend-boundaries))
      inside?    (partial g3/inside? boundaries)
      discover   (partial discover inside? droplets)]
  (->> (flood discover (first boundaries))
       (reduce (fn [acc w]
                 (+ acc (count (filter droplets (g3/neighbors w)))))
               0))) ; 2028
