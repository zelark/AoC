(ns zelark.aoc-2023.day-10
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.graph :as g]
            [zelark.aoc.grid-2d :as g2]
            [medley.core :as mdl]))

;; --- Day 10: Pipe Maze ---
;; https://adventofcode.com/2023/day/10

(def input (aoc/get-input 2023 10))

;; F | 7
;; - S -
;; L | J

(def conn->pipe
  {#{g2/up    g2/down}  \|, #{g2/right g2/left}  \-
   #{g2/up    g2/right} \L, #{g2/up    g2/left}  \J
   #{g2/down  g2/left}  \7, #{g2/down  g2/right} \F})

(defn find-direction [maze loc]
  (let [conn (->> (map #(-> [%1 (set %2)])
                       [g2/up g2/right g2/down g2/left]
                       (partition 3 2 [\F \| \7 \- \J \| \L \- \F]))
                  (keep (fn [[dir conn?]]
                          (when-let [tile (maze (g2/plus loc dir))]
                            (when (conn? tile) dir))))
                  (set))]
    [(first conn) (conn->pipe conn)]))

(defn find-start [maze]
  (key (mdl/find-first (fn [[_ ch]] (when (= ch \S) ch)) maze)))

(defn step [maze loc dir]
  (let [loc+ (g2/plus loc dir)
        tile (maze loc+)]
    (if (contains? #{\| \- \S} tile)
      [loc+ dir]
      (condp = [dir tile]
        [g2/up    \7] [loc+ g2/left]
        [g2/up    \F] [loc+ g2/right]
        [g2/right \7] [loc+ g2/down]
        [g2/right \J] [loc+ g2/up]
        [g2/down  \J] [loc+ g2/left]
        [g2/down  \L] [loc+ g2/right]
        [g2/left  \L] [loc+ g2/up]
        [g2/left  \F] [loc+ g2/down]))))

(defn loop-points [maze start dir]
  (loop [loc start
         dir dir
         pts [start]]
    (let [[loc+ dir+] (step maze loc dir)]
      (if (= loc+ start)
        pts
        (recur loc+ dir+ (conj pts loc+))))))

;; part 1 (39.027 msecs)
(let [maze  (g2/parse input)
      start (find-start maze)
      [dir] (find-direction maze start)]
  (quot (count (loop-points maze start dir)) 2)) ; 6909

;; part 2
(defn point-inside?
  "It uses ray casting algorithm.
  https://en.wikipedia.org/wiki/Point_in_polygon
  
  L-7, F-J, and | are counting edges;
  L-J, F-7, - are not counting."
  [maze loop? [x y]]
  (->> (for [x*    (range x)
             :let  [point [x* y]]
             :when (loop? point)]
         (maze point))
       (apply str)
       (re-seq #"L\-*7|F\-*J|\|")
       (count)
       (odd?)))

(time ;; (110.585 msecs)
 (let [maze       (g2/parse input)
       start      (find-start maze)
       [dir pipe] (find-direction maze start)
       maze       (assoc maze start pipe)
       loop?      (set (loop-points maze start dir))
       maze-without-loop (mdl/remove-keys loop? maze)
       neighbors  #(filter maze-without-loop (g2/neighbors %))]
   (->> (g/connected-groups maze-without-loop neighbors)
        (reduce (fn [acc group]
                  (if (point-inside? maze loop? (first group)) ; Check first point from a group.
                    (+ acc (count group))
                    acc))
                0))))

;; vizualisation (part 2)
(let [maze  (g2/parse input)
      start (find-start maze)
      [dir pipe] (find-direction maze start)
      maze' (assoc maze start pipe)
      loop? (set (loop-points maze' start dir))
      adjecent (fn [loc]
                 (->> (g2/neighbors loc)
                      (filter #(and (maze' %) (not (loop? %))))))
      pretty-map {\F \u256D, \7 \u256E
                  \L \u2570, \J \u256F
                  \| \u2502, \- \u2500
                  \S \u25BD}
      inside? (->> (g/connected-groups (mdl/remove-keys loop? maze') adjecent)
                   (reduce (fn [acc group]
                             (if (point-inside? maze' loop? (first group))
                               (into acc group)
                               acc))
                           #{}))]
  (->> (reduce-kv (fn [m pt tile]
                    (cond
                      (loop? pt)   (assoc m pt (pretty-map tile))
                      (inside? pt) (assoc m pt \u2022)
                      :else (assoc m pt \u25EF)))
                  maze
                  maze)
       (aoc/print-points-2)))
