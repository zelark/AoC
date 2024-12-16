(ns zelark.aoc-2024.day-16
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [clojure.data.priority-map :refer [priority-map]]))

;; --- Day 16: Reindeer Maze ---
;; https://adventofcode.com/2024/day/16

(def input (aoc/get-input 2024 16))

(defn parse-input [input]
  (let [maze  (g2/parse input (g2/any-but g2/empty-space))
        start (some #(when (= (val %) \S) (key %)) maze)
        end   (some #(when (= (val %) \E) (key %)) maze)]
    {:start start
     :end   end
     :maze  (dissoc maze start end)}))

(defn best-spots [graph dist start goal?]
  (loop [seen  {}
         queue (priority-map [start] 0)
         min-score Long/MAX_VALUE
         best-points #{}]
    (if (seq queue)
      (let [[path score] (peek queue)
            current      (peek path)]
        (if (goal? current)
          (if (<= score min-score)
            (recur seen (pop queue) score (into best-points (map first path)))
            (count best-points))
          (if (< (seen current Long/MAX_VALUE) score)
            (recur seen (pop queue) min-score best-points)
            (let [seen'  (assoc seen current score)
                  queue' (reduce (fn [q target]
                                   (assoc q
                                          (conj path target)
                                          (+ score (dist current target))))
                                 (pop queue)
                                 (graph current))]
              (recur seen' queue' min-score best-points)))))
      (count best-points))))

(defn neighbours [maze]
  (fn [[loc dir]]
    (let [new-loc (g2/plus loc dir)]
      (cond-> []
        (nil? (maze new-loc)) (conj [new-loc dir])
        :turn-right (conj [loc (g2/turn :right dir)])
        :turn-left  (conj [loc (g2/turn :left dir)])))))

(defn solve [part input]
  (let [{:keys [start end maze]} (parse-input input)
        start [start g2/right]
        goal? #(= (first %) end)
        dist  (fn [[_ d1] [_ d2]] (if (= d1 d2) 1 1000))
        h     (constantly 1)]
    (if (= part :p2)
      (best-spots (neighbours maze) dist start goal?)
      (aoc/astar (neighbours maze) dist h start goal? {:score? true}))))

;; part 1 (281.778804 msecs)
(solve :p1 input) ; 147628

;; part 2 (6120.481336 msecs)
(solve :p2 input) ; 670
