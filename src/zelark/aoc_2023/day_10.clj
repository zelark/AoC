(ns zelark.aoc-2023.day-10
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [medley.core :as mdl]
            [clojure.set :as set]))

;; --- Day 10: Pipe Maze ---
;; https://adventofcode.com/2023/day/10

(def input (aoc/get-input 2023 10))

(def dir->delta
  {:west  [-1  0], :east  [1 0]
   :north [ 0 -1], :south [0 1]})

(def opposite-dir
  {:north :south
   :south :north
   :west  :east
   :east  :west})

(def tile->dirs
  {\| #{:north :south}
   \- #{:east  :west}
   \L #{:north :east}
   \J #{:north :west}
   \7 #{:south :west}
   \F #{:south :east}})

(defn neigbors [maze [x y :as loc]]
  (let [tile (maze loc)
        dirs (tile->dirs tile)]
    (->> (map dir->delta dirs)
         (map (fn [[dx dy]] [(+ x dx) (+ y dy)]))
         (keep #(when (maze %) %)))))

(defn loop-points [graph start]
  (:seen (aoc/bfs graph start (constantly false))))

(defn find-start [maze]
  (key (mdl/find-first (fn [[_ ch]] (when (= ch \S) ch)) maze)))

(defn determine-shape [maze [x y]]
  (let [dirs->tile (set/map-invert tile->dirs)]
    (->> (for [[dir [dx dy]] dir->delta
               :let [tile (maze [(+ x dx) (+ y dy)])]
               :when tile
               :when (contains? (tile->dirs tile) (opposite-dir dir))]
           dir)
     (set)
     (dirs->tile))))

;; part 1
(let [maze  (g2/parse input identity)
      start (find-start maze)
      shape (determine-shape maze start)
      maze' (assoc maze start shape)]
  (-> (loop-points (partial neigbors maze') start)
      (count)
      (quot 2))) ; 6909

;; part 2
(defn point-inside?
  "It uses ray casting algorithm.
  https://en.wikipedia.org/wiki/Point_in_polygon
  
  L-7, F-J, and | are counting edges;
  L-J, F-7, - are not counting."
  [maze loop? [x y]]
  (->> (for [xx (range x)
             :let  [point [xx y]]
             :when (loop? point)]
         (maze point))
       (apply str)
       (re-seq #"L\-*7|F\-*J|\|")
       (count)
       (odd?)))

(let [maze  (g2/parse input identity)
      start (find-start maze)
      shape (determine-shape maze start)
      maze' (assoc maze start shape)
      the-loop (loop-points (partial neigbors maze') start)]
  (-> (for [point (keys maze')
            :when (not (the-loop point))
            :when (point-inside? maze' the-loop point)]
        point)
      (count))) ; 461
