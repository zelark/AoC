(ns zelark.aoc-2024.day-20
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [zelark.aoc.graph :as g]))

;; --- Day 20: Race Condition ---
;; https://adventofcode.com/2024/day/20

(def input (aoc/get-input 2024 20))

(defn parse-input [input]
  (let [maze  (g2/parse input (g2/any-but g2/empty-space))
        start (some #(when (= (val %) \S) (key %)) maze)
        end   (some #(when (= (val %) \E) (key %)) maze)]
    {:start start
     :end   end
     :walls (disj (set (keys maze)) start end)}))

(defn neighbours [walls]
  (fn [loc]
    (->> (g2/neighbors loc)
         (remove walls))))

(defn find-cheats [from locations limit]
  (for [to (drop limit locations)
        :let [dist (aoc/manhattan-distance from to)]
        :when (<= 2 dist limit)]
    [to dist]))

(defn solve [part input]
  (let [{:keys [walls start end]} (parse-input input)
        path    (g/bfs (neighbours walls) start end)
        picos   (count (rest path))
        loc->ps (zipmap path (iterate dec picos))
        limit   ({:p1 2 :p2 20} part)
        saved   100]
    (loop [result 0
           ps     0
           path   path]
      (if (seq (drop limit path))
        (let [[loc & rpath] path
              cheats (find-cheats loc rpath limit)
              result (reduce (fn [k [c d]]
                               (if (<= (+ ps d (loc->ps c)) (- picos saved))
                                 (inc k)
                                 k))
                             result cheats)]
          (recur result (inc ps) rpath))
        result))))

;; part 1 (1858.092578 msecs)
(time (solve :p1 input)) ; 1365

;; part 2 (2076.957532 msecs)
(time (solve :p2 input)) ; 986082
