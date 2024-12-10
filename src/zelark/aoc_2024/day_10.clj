(ns zelark.aoc-2024.day-10
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [zelark.aoc.graph :as g]))

;; --- Day 10: Hoof It ---
;; https://adventofcode.com/2024/day/10

(def input (aoc/get-input 2024 10))

(def lowest? zero?)
(def highest? #{9})

(defn parse-input [input]
  (g2/parse input identity aoc/ch->digit))

(defn find-trailheads [grid]
  (keys (filter (fn [[_ v]] (lowest? v)) grid)))

(defn moves [grid from]
  (for [to (g2/neighbors from)
        :when (= (inc (grid from)) (grid to))]
    to))

(defn trailhead-score [grid loc]
  (->> (g/bfs #(moves grid %) loc)
       (filter #(highest? (grid (peek %))))
       (count)))

(defn solve [part input]
  (let [grid (parse-input input)
        trailheads (find-trailheads grid)]
    (transduce (map #(part grid %)) + trailheads)))

;; part 1 (20.51833 msecs)
(solve trailhead-score input) ; 510

;; part 2 (26.975456 msecs)
(defn seacrh [neighbors stack]
  (when-let [path (peek stack)]
    (cons path
          (lazy-seq (seacrh neighbors
                            (->> (neighbors (peek path))
                                 (reduce #(conj %1 (conj path %2))
                                         (pop stack))))))))

(defn trailhead-rating [grid loc]
  (->> (seacrh #(moves grid %) [[loc]])
       (filter #(highest? (grid (peek %))))
       (count)))

(solve trailhead-rating input) ; 1058
