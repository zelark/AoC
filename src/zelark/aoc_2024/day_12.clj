(ns zelark.aoc-2024.day-12
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [zelark.aoc.graph :as g]))

;; --- Day 12: Garden Groups ---
;; https://adventofcode.com/2024/day/12

(def input (aoc/get-input 2024 12))

(defn parse-input [input]
  (let [grid (g2/parse input)
        neighbors (fn [loc]
                    (let [plot (grid loc)]
                      (->> (g2/neighbors loc)
                           (filter #(= (grid %) plot)))))]
    {:grid    grid
     :regions (g/connected-groups grid neighbors)}))

;; part 1 (300.680245 msecs)
(defn calc-perimeter [grid plot region]
  (let [cuont-sides (fn [loc]
                      (->> (g2/neighbors loc)
                           (filter #(not= (grid %) plot))
                           (count)))]
    (aoc/sum cuont-sides region)))

(let [{:keys [grid regions]} (parse-input input)]
  (reduce (fn [acc region]
            (let [plot (grid (first region))
                  area (count region)]
              (+ acc (* area (calc-perimeter grid plot region)))))
          0
          regions)) ; 1533024

;; part 2 (445.157442 msecs)
(defn calc-sides
  "Walks around region and counts its outer sides."
  [region]
  (let [start (g2/plus (first (sort-by (juxt second first) region)) g2/up)]
    (loop [direction g2/right
           current   start
           sides     0
           path      [current]]
      (if (and (= current start)
               (pos? sides))
        [sides path]
        (let [next (g2/plus current direction)]
          (if (not (region next))
            (let [direction' (g2/turn :right direction)
                  next'      (g2/plus next direction')]
              (if (not (region next'))
                (recur direction' next' (inc sides) (conj path next'))
                (recur direction next sides (conj path next))))
            (recur (g2/turn :left direction) current (inc sides) path)))))))

(let [{:keys [regions]} (parse-input input)
      regions (->> regions
                   (reduce (fn [acc region]
                             (let [area (count region)
                                   [sides boundary] (calc-sides region)]
                               (conj acc
                                     {:area area
                                      :sides sides
                                      :boundary boundary
                                      :locations region
                                      :price (* area sides)})))
                           []))
      outer-price (aoc/sum :price regions)
      inner-price (->> (for [r1 regions r2 regions :when (not= r1 r2)]
                         (if (every? (r1 :locations) (r2 :boundary))
                           (* (r1 :area) (r2 :sides))
                           0))
                       (aoc/sum))]
  (+ outer-price inner-price)) ; 910066
