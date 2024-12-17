(ns zelark.aoc-2024.day-16-fast
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

(defn backtrack [node seen]
  (loop [route []
         nodes [node]]
    (if (empty? nodes)
      (into [] (comp (map first) (distinct)) route)
      (recur (into route nodes)
             (mapcat (comp :from seen) nodes)))))

(defn neighbours [maze]
  (fn [[loc dir] score]
    (let [east (g2/turn :right dir)
          west (g2/turn :left dir)]
      (->> [[[(g2/plus loc east) east] (+ score 1000 1)]
            [[(g2/plus loc west) west] (+ score 1000 1)]
            [[(g2/plus loc dir)  dir]  (+ score 1)]]
           (remove #(-> % ffirst maze))))))

(defn solve [part input]
  (let [{:keys [start end maze]} (parse-input input)
        start                    [start g2/right]
        end                      [end g2/up]
        neighbours               (neighbours maze)]
    (loop [queue (priority-map start 0)
           seen  {}]
      (if (seq queue)
        (let [[current current-score] (peek queue)
              [queue' seen']
              (->> (neighbours current current-score)
                   (reduce (fn [[q s] [node score]]
                             (let [seen-score (get-in s [node :score] Long/MAX_VALUE)]
                               (if (<= score seen-score)
                                 (let [from (get-in s [node :from] [])]
                                   [(assoc q node score)
                                    (assoc s node {:from (conj from current) :score score})])
                                 [q s])))
                           [(pop queue) seen]))]
          (recur queue' seen'))
        (let [locations (backtrack end seen)]
          (get {:p1 (get-in seen [end :score])
                :p2 (count locations)}
               part))))))

;; part 1 (138.716991 msecs)
(solve :p1 input) ; 147628

;; part 2 (141.093461 msecs)
(solve :p2 input) ; 670
