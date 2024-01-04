(ns zelark.aoc-2022.day-12
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.graph :as g]
            [clojure.string :as str]))

;; --- Day 12: Hill Climbing Algorithm ---
;; https://adventofcode.com/2022/day/12

(def input (aoc/get-input 2022 12))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (map-indexed (fn [x ch] [[x y] ch]) line)) (range))
       (reduce (fn [m [loc h]]
                 (case h
                   \S (assoc m loc \a, :start loc)
                   \E (assoc m loc \z, :end loc)
                   (assoc m loc h)))
               {})))

(defn moves [heightmap can-move? [x y :as src]]
  (for [dst [[x (dec y)] [(inc x) y] [x (inc y)] [(dec x) y]]
        :when (heightmap dst)
        :when (can-move? (heightmap src) (heightmap dst))]
    dst))

(def heightmap (parse-input input))

;; part 1
(let [can-move? (fn [p1 p2] (<= (- (int p2) (int p1)) 1))]
  (->> (g/bfs (partial moves heightmap can-move?)
              (:start heightmap)
              (:end heightmap))
       (count)
       (dec))) ; 528

;; part 2 (There is a trick, we can do reverse search from goal to the nearest start).
(let [goal? (reduce-kv (fn [acc loc h]
                         (if (= h \a) (conj acc loc) acc))
                       #{}
                       heightmap)
      can-move? (fn [p1 p2] (<= (- (int p1) (int p2)) 1))]
  (->> (g/bfs (partial moves heightmap can-move?)
              (:end heightmap)
              goal?)
       (count)
       (dec))) ; 522
