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
     :entrance  [(inc min-x) min-y]
     :goal      [(dec max-x) max-y]}))

(defn move-blizzard [mx my [x y] blizzard]
  (case blizzard
    \^ [x (aoc/mod-1 (dec y) my)]
    \v [x (aoc/mod-1 (inc y) my)]
    \< [(aoc/mod-1 (dec x) mx) y]
    \> [(aoc/mod-1 (inc x) mx) y]))

(defn blow [mx my pos->blizzards]
  (reduce-kv (fn [m pos blizzards]
               (reduce (fn [m blizzard]
                         (let [new-pos (move-blizzard mx my pos blizzard)]
                           (update m new-pos conj blizzard)))
                       m blizzards))
             {} pos->blizzards))

(defn dodge [{:keys [start goal minute] :as state} moves]
  (loop [positions [start]
         minute minute]
    (if (some #{goal} positions)
      (assoc state :minute minute)
      (recur (set (for [cpos positions
                        npos (moves (inc minute) cpos)]
                    npos))
             (inc minute)))))

(defn solve [part input]
  (let [{:keys [walls blizzards bounds entrance goal]} (parse input)
        [_ [bmx bmy]] (g2/narrow-boundaries bounds)
        blow (partial blow bmx bmy)
        max-minute (aoc/lcm bmx bmy) ; After the minute blizzards will get initial state.
        minute->blizzards (->> (iterate blow blizzards)
                               (take max-minute)
                               (vec))
        moves (fn [minute pos]
                (let [blizzards (nth minute->blizzards (mod minute max-minute))]
                  (for [dir [[0 -1] [+1 0] [0 +1] [-1 0] [0 0]]
                        :let [new-pos (g2/plus pos dir)]
                        :when (and (g2/in-bounds? bounds new-pos)
                                   (not (walls new-pos))
                                   (not (blizzards new-pos)))]
                    new-pos)))
        init-state {:start entrance
                    :goal goal
                    :minute 0}
        back #(set/rename-keys % {:start :goal :goal :start})]
    (->> (iterate #(-> % (dodge moves) back) init-state)
         (drop ({:p1 1 :p2 3} part part))
         (first)
         :minute)))

;; part 1
(solve :p1 input) ; => 228

;; part 2
(solve :p2 input) ; => 723
