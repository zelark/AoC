(ns zelark.aoc-2022.day-10
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 10: Cathode-Ray Tube ---
;; https://adventofcode.com/2022/day/10

(def input (aoc/get-input 2022 10))

(defn run [code f]
  (loop [state {:x 1, :mem []}, ip 0, cycle 0]
    (let [[op x] (nth code ip [:hlt])]
      (case op
        :hlt (:mem state)
        :noop (let [cycle (inc cycle)
                    state (f state cycle)]
                (recur state (inc ip) cycle))
        :addx (let [cycle (inc cycle)
                    state (f state cycle)
                    cycle (inc cycle)
                    state (f state cycle)]
               (recur (update state :x + x) (inc ip) cycle))))))

;; part 1
(defn signal-strength [state cycle]
  (cond-> state
    (contains? #{20 60 100 140 180 220} cycle)
    (update :mem conj (* cycle (:x state)))))

(->> (run (aoc/parse-asm-code input) signal-strength)
     (reduce +)) ; 11720

;; part 2
(defn draw-pixel [{:keys [x] :as state} cycle]
  (let [pos (mod (dec cycle) 40)]
    (if (some #{pos} [(dec x) x (inc x)])
      (update state :mem conj \#)
      (update state :mem conj \.))))

(->> (run (aoc/parse-asm-code input) draw-pixel)
     (partition 40)
     (map #(str/join %))) ; ERCREPCJ

;; ("####.###...##..###..####.###...##....##."
;;  "#....#..#.#..#.#..#.#....#..#.#..#....#."
;;  "###..#..#.#....#..#.###..#..#.#.......#."
;;  "#....###..#....###..#....###..#.......#."
;;  "#....#.#..#..#.#.#..#....#....#..#.#..#."
;;  "####.#..#..##..#..#.####.#.....##...##..")
