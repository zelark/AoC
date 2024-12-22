(ns zelark.aoc-2024.day-22
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 22: Monkey Market ---
;; https://adventofcode.com/2024/day/22

(def input (aoc/get-input 2024 22))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-long)))

(def mix bit-xor)
(def prune #(bit-and % 16777215)) ; 2^24 - 1

(defn next-number [n]
  (let [a (-> (bit-shift-left n 6) (mix n) prune)
        b (-> (bit-shift-right a 5) (mix a) prune)]
    (-> (bit-shift-left b 11) (mix b) prune)))

(defn secret-number-seq [seed]
  (iterate next-number seed))

;; part 1 (206.050451 msecs)
(->> (parse-input input)
     (aoc/sum #(nth (secret-number-seq %) 2000))) ; 16953639210

;; part 2 (12660.11716 msecs)
(defn seqs-to-prices [buyer]
  (let [prices  (->> (secret-number-seq buyer)
                     (take 2001)
                     (map #(mod % 10)))
        changes (map - (rest prices) prices)]
    (->> (map vector (partition 4 1 changes) (drop 4 prices))
         (reduce (fn [m [k v]] (if (m k) m (assoc m k v))) {}))))

(->> (parse-input input)
     (pmap seqs-to-prices)
     (apply merge-with +)
     (apply max-key val)
     (val)) ; 1863
