(ns zelark.aoc-2024.day-07
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

;; --- Day 7: Bridge Repair ---
;; https://adventofcode.com/2024/day/7

(def input (aoc/get-input 2024 7))

(defn || [a b] (parse-long (str a b)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map aoc/parse-longs)))

(defn calc [[a b & numbers] [o & operators]]
  (->> (map vector numbers operators)
       (reduce (fn [acc [num op]] (op acc num)) (o a b))))

(defn can-be-true? [operators [value & numbers]]
  (let [n (count numbers)]
    (boolean (->> (combo/selections operators (dec n))
                  (map #(calc numbers %))
                  (some #{value})))))

(defn solve [input operators]
  (->> (parse-input input)
       (filter (partial can-be-true? operators))
       (aoc/sum first)))

;; part 1 (510.623753 msecs)
(solve input [+ *]) ; 4555081946288

;; part 2 (23129.871037 msecs)
(solve input [+ * ||]) ; 227921760109726
