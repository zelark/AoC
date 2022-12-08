(ns zelark.aoc-2022.day-08
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 8: Treetop Tree House ---
;; https://adventofcode.com/2022/day/8

(def input (aoc/get-input 2022 8))

(defn parse-line [line]
  (->> (re-seq #"\d" line)
       (mapv parse-long)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv parse-line)))

(def grid (parse-input input))

(defn iter [f axis [x y]]
  (cond
    (= axis :x) (map #(-> [% y]) (rest (iterate f x)))
    (= axis :y) (map #(-> [x %]) (rest (iterate f y)))))

(defn related-trees [grid loc]
  (let [xf (comp (map #(aoc/grid-get grid %))
                 (take-while some?))]
    [(eduction xf (iter dec :y loc))
     (eduction xf (iter inc :x loc))
     (eduction xf (iter inc :y loc))
     (eduction xf (iter dec :x loc))]))

;; part 1
(defn visible? [grid loc]
  (let [height (aoc/grid-get grid loc)
        taller? #(<= height %)]
    (->> (related-trees grid loc)
         (map #(some taller? %))
         (some nil?)
         (boolean))))

(let [max-x (count (first grid))
      max-y (count grid)]
  (->> (for [x (range max-x)
             y (range max-y)
             :when (visible? grid [x y])]
         [x y])
       (count))) ; 1676

;; part 2
(defn viewing-distance [height trees]
  (reduce #(if (<= height %2) (reduced (inc %1)) (inc %1)) 0 trees))

(defn scenic-score [grid loc]
  (let [height (aoc/grid-get grid loc)]
    (->> (related-trees grid loc)
         (map #(viewing-distance height %))
         (reduce *))))

(let [max-x (count (first grid))
      max-y (count grid)]
  (->> (for [x (range max-x)
             y (range max-y)]
         (scenic-score grid [x y]))
       (apply max))) ; 313200
