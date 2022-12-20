(ns zelark.aoc-2022.day-20
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.circular-list :as clist]))

;; --- Day 20: Grove Positioning System ---
;; https://adventofcode.com/2022/day/20

(def input (aoc/get-input 2022 20))

(defn move-number [node n]
  (when (pos? n)
    (-> (iterate clist/get-next node)
        (nth n)
        (clist/move-after node))))

(defn mix [nodes]
  (let [len (count nodes)
        m   (dec len)]
    (doseq [node nodes
            :let [n (mod (clist/value node) m)]]
      (move-number node n))))

(defn grove-coords [head]
  (let [xs (iterate clist/get-next head)]
    (mapv #(clist/value (nth xs %)) [1000 2000 3000])))

(defn solve
  ([input] (solve input nil 1))
  ([input decription-key times]
   (let [numbers (cond->> (aoc/parse-longs input)
                   decription-key (map #(* decription-key %)))
         nodes   (clist/circular-list numbers)
         zero    (nth nodes (.indexOf numbers 0))]
     (dotimes [_ times] (mix nodes))
     (aoc/sum (grove-coords zero)))))

;; part 1
(solve input) ; => 8302

;; part 2
(solve input 811589153 10) ; => 656575624777
