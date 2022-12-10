(ns zelark.aoc-2022.day-10
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 10: Cathode-Ray Tube ---
;; https://adventofcode.com/2022/day/10

(def input (aoc/get-input 2022 10))

(defn run [code f]
  (->> code
       (reduce (fn [state [op x]]
                 (case op
                   :noop (f (update state :cycle inc))
                   :addx (let [state (f (update state :cycle inc))
                               state (f (update state :cycle inc))]
                           (update state :x + x))))
               {:cycle 0, :x 1, :mem []})
       :mem))

;; part 1
(defn signal-strength [{:keys [x cycle] :as state}]
  (cond-> state
    (contains? #{20 60 100 140 180 220} cycle)
    (update :mem conj (* cycle x))))

(->> (run (aoc/parse-asm-code input) signal-strength)
     (reduce +)) ; 11720

;; part 2
(defn draw-pixel [{:keys [x cycle] :as state}]
  (let [pos (mod (dec cycle) 40)]
    (if (some #{pos} [(dec x) x (inc x)])
      (update state :mem conj \#)
      (update state :mem conj \.))))

(->> (run (aoc/parse-asm-code input) draw-pixel)
     (partition 40)
     (map #(str/join %))) ; ERCREPCJ
