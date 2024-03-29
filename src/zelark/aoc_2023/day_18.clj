(ns zelark.aoc-2023.day-18
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [clojure.edn :as edn]))

;; --- Day 18: Lavaduct Lagoon ---
;; https://adventofcode.com/2023/day/18

(def input (aoc/get-input 2023 18))

(defn parse [input]
  (->> (re-seq #"([UDLR]) (\d+) \(#([0-9a-f]{6})\)" input)
       (map (fn [[_ dir n hex]] [(keyword dir) (parse-long n) hex]))))

(def direction {:R g2/right :D g2/down :L g2/left :U g2/up})

(defn trench-points [dig-plan]
  (let [mult (fn [[x y] n] [(* x n) (* y n)])]
    (reductions (fn [loc [d n]] (g2/plus loc (mult (direction d) n)))
                [0 0]
                dig-plan)))

(defn decode [hex]
  [([:R :D :L :U] (bit-and hex 0xF))
   (bit-shift-right (bit-and hex 0xFFFFF0) 4)])

(defn correct [dig-plan]
  (map (fn [[_ _ hex]] (-> (str "0x" hex) edn/read-string decode)) dig-plan))

(defn solve [input & {:keys [correct-plan?]}]
  (let [plan   (cond-> (parse input) correct-plan? correct)
        trench (trench-points plan)
        p      (g2/perimeter-of-polygon trench)
        a      (g2/area-of-polygon trench)]
    (inc (+ a (/ p 2)))))

;; part 1 (7.222099 msecs)
(solve input) ; 48400

;; part 2 (10.61384 msecs)
(solve input :correct-plan? true) ; 72811019847283
