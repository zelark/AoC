(ns zelark.aoc-2024.day-23
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [zelark.aoc.graph :as g]
            [clojure.math.combinatorics :as combo]))

;; --- Day 23: LAN Party ---
;; https://adventofcode.com/2024/day/23

(def input (aoc/get-input 2024 23))

(defn parse-input [input]
  (->> (str/split-lines input)
       (reduce (fn [m line]
                 (let [[c & cs] (re-seq #"[a-z]+" line)]
                   (reduce #(-> (update %1 c (fnil conj #{}) %2)
                                (update %2 (fnil conj #{}) c))
                           m cs)))
               {})))


;; part 1 (50.280547 msecs)
(->> (g/maximal-cliques (parse-input input))
     (mapcat #(combo/combinations % 3))
     (filter (fn [v] (some #(= (first %) \t) v)))
     (count)) ; 1075

;; part 2 (22.58502 msecs)
(->> (g/maximal-cliques (parse-input input))
     (apply max-key count)
     (sort)
     (str/join ",")) ; az,cg,ei,hz,jc,km,kt,mv,sv,sx,wc,wq,xy
