(ns zelark.aoc-2023.day-25
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.graph :as g]
            [clojure.string :as str]))

;; --- Day 25: Snowverload ---
;; https://adventofcode.com/2023/day/25

(def input (aoc/get-input 2023 25))

(defn parse [input]
  (->> (str/split-lines input)
       (reduce (fn [m line]
                 (let [[c & cs] (re-seq #"[a-z]+" line)]
                   (reduce #(-> (update %1 c (fnil conj #{}) %2)
                                (update %2 (fnil conj #{}) c))
                           m cs)))
               {})))

(defn edge-flows [graph v]
  (->> (g/bfs graph v)
       (next)
       (map #(frequencies (map set (partition 2 1 %))))
       (apply merge-with +)))

(defn remove-edge [g edge]
  (let [[a b] (vec edge)]
    (-> g
        (update a disj b)
        (update b disj a))))

;; part 1 (7576.346 msecs)
(let [graph (parse input)]
  (->> (keys graph)
       (pmap (partial edge-flows graph))
       (apply merge-with +)
       (sort-by val >)
       (map first)
       (take 3)
       (reduce remove-edge graph)
       (g/connected-groups)
       (aoc/mul count))) ; 582692

;; part 2
;; PRESS THE BUTTON

