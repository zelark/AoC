(ns zelark.aoc-2023.day-07
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 7: Camel Cards ---
;; https://adventofcode.com/2023/day/7

(def input (aoc/get-input 2023 07))

(def ^:dynamic *part2* nil)
(def ^:dynamic *cards* nil)

(defn parse-line [line]
  (let [[hand bid] (str/split line #"\s")]
    [hand (parse-long bid)]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)))

(def hand-type->strength
  {[1 1 1 1 1] 1   ; High card
   [2 1 1 1]   2   ; One pair
   [2 2 1]     3   ; Two pair
   [3 1 1]     4   ; Three of a kind
   [3 2]       5   ; Full house
   [4 1]       6   ; Four of a kind
   [5]         7}) ; Five of a kind

(defn hand-strength [hand]
  (let [freq   (frequencies hand)
        jokers (get freq \J 0)]
    (hand-type->strength
     (if (and *part2* (<= 1 jokers 4))
       (update (vec (sort > (vals (dissoc freq \J)))) 0 + jokers)
       (sort > (vals freq))))))

(defn hand-cmp [h1 h2]
  (let [hs1 (hand-strength h1)
        hs2 (hand-strength h2)]
    (if (identical? hs1 hs2)
      (->> (map #(compare (*cards* %1) (*cards* %2)) h1 h2)
           (drop-while zero?)
           (first))
      (compare hs1 hs2))))

(defn solve [input]
  (->> (parse-input input)
       (sort-by first hand-cmp)
       (map-indexed (fn [rank [_ bid]] (* bid (inc rank))))
       (aoc/sum)))

;; part 1 (63.6629 msecs)
(binding [*cards* (zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range))]
  (solve input)) ; 248836197

;; part 2 (67.305929 msecs)
(binding [*part2* true
          *cards* (zipmap [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A] (range))]
  (solve input)) ; 251195607
