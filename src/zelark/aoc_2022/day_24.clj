(ns zelark.aoc-2022.day-24
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [clojure.set :as set]))

;; --- Day 24: Blizzard Basin ---
;; https://adventofcode.com/2022/day/24

(def input (aoc/get-input 2022 24))

(defn parse [input]
  (let [walls (g2/parse input #{\#})
        blizzards (g2/parse input #{\> \< \^ \v})
        [[min-x min-y] [max-x max-y] :as bounds] (g2/boundaries walls)]
    {:walls     walls
     :blizzards (update-vals blizzards list)
     :bounds    bounds
     :start     [(inc min-x) min-y]
     :goal      [(dec max-x) max-y]}))

(defn move-blizzard [mx my [x y] blizzard]
  (case blizzard
    \^ [x (aoc/mod-1 (dec y) my)]
    \v [x (aoc/mod-1 (inc y) my)]
    \< [(aoc/mod-1 (dec x) mx) y]
    \> [(aoc/mod-1 (inc x) mx) y]))

(defn blow [mx my blizzards]
  (reduce-kv (fn [m1 loc blzs]
               (reduce (fn [m2 b]
                         (let [new-loc (move-blizzard mx my loc b)]
                           (update m2 new-loc conj b))) m1 blzs))
             {}
             blizzards))

(defn moves [bounds walls blizzards pos]
  (for [dir [[0 -1] [+1 0] [0 +1] [-1 0] [0 0]]
        :let [new-pos (g2/plus pos dir)]
        :when (and (g2/in-bounds? bounds new-pos)
                   (not (walls new-pos))
                   (not (blizzards new-pos)))]
    new-pos))

(defn dodge [{:keys [start goal minute blizzards] :as state} moves]
  (loop [blizzards blizzards
         positions [start]
         minute minute]
    (if (some #{goal} positions)
      (assoc state :minute minute, :blizzards blizzards)
      (recur (next blizzards)
             (set (for [cpos positions
                        npos (moves (first blizzards) cpos)]
                    npos))
             (inc minute)))))

(defn solve [part input]
  (let [{:keys [walls blizzards bounds start goal]} (parse input)
        [_ [bmx bmy]] (g2/narrow-boundaries bounds)
        blow  (partial blow bmx bmy)
        moves (partial moves bounds walls)
        init-state {:start start
                    :goal goal
                    :minute 0
                    :blizzards (next (iterate blow blizzards))}
        swap #(set/rename-keys % {:start :goal :goal :start})]
    (->> (iterate #(-> % (dodge moves) swap) init-state)
         (drop ({:p1 1 :p2 3} part))
         (first)
         :minute)))

;; part 1
(solve :p1 input) ; => 228

;; part 2
(solve :p2 input) ; => 723
