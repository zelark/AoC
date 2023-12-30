(ns zelark.aoc-2023.day-14
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 14: Parabolic Reflector Dish ---
;; https://adventofcode.com/2023/day/14

(def input (aoc/get-input 2023 14))

(defn parse [input]
  (str/split-lines input))

(defn tilt-part [^String part dir]
  (if (str/blank? part)
    part
    (let [rocks (.repeat "O" (aoc/cnt part \O))
          space (.repeat "." (aoc/cnt part \.))]
      (case dir
        :left  (str rocks space)
        :right (str space rocks)))))

(defn tilt-row [^String row dir]
  (->> (str/split row #"#" -1)
       (map #(tilt-part % dir))
       (str/join "#")))

(defn tilt [platform dir]
  (mapv #(tilt-row % dir) platform))

(defn calc-load [platform]
  (->> (map #(* (aoc/cnt %1 \O) %2) platform (range (count platform) 0 -1))
       (aoc/sum)))

;; part 1 (3.449105 msecs)
(let [panel (parse input)]
  (-> panel aoc/rotate-ccw (tilt :left) aoc/rotate-cw calc-load)) ; 105623

;; part 2 (730.663208 msecs)
(defn spin-cycle [platform]
  (-> platform
      aoc/rotate-ccw (tilt :left)    ; north
      aoc/rotate-ccw (tilt :right)   ; west
      aoc/rotate-ccw (tilt :left)    ; south
      aoc/rotate-ccw (tilt :right))) ; east

(defn final-state [start end state->cycle-id]
  (let [left (mod (- 1000000000 end) (- end start))
        id (+ start left)]
    ((set/map-invert state->cycle-id) id)))

(let [panel (parse input)]
  (->> (iterate spin-cycle panel)
       (map-indexed vector)
       (reduce (fn [seen [cycle-id state]]
                 (if-let [start (seen state)]
                   (reduced (final-state start cycle-id seen))
                   (assoc seen state cycle-id)))
               {})
       (calc-load))) ; 98029
