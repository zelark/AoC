(ns zelark.aoc-2022.day-02
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 2: Rock Paper Scissors ---
;; https://adventofcode.com/2022/day/2

(def input (aoc/get-input 2022 02))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))))

(def letter->shape
  {"A" :rock "B" :paper "C" :scissors
   "X" :rock "Y" :paper "Z" :scissors})

(defn score [choice]
  (case choice
    ;; Draw
    [:rock :rock]         [(+ 1 3) (+ 1 3)]
    [:paper :paper]       [(+ 2 3) (+ 2 3)]
    [:scissors :scissors] [(+ 3 3) (+ 3 3)]
    ;; First player wins 
    [:rock :scissors]     [(+ 1 6) (+ 0 3)]
    [:paper :rock]        [(+ 2 6) (+ 0 1)]
    [:scissors :paper]    [(+ 3 6) (+ 0 2)]
    ;; Second player wins
    [:scissors :rock]     [(+ 0 3) (+ 1 6)]
    [:rock :paper]        [(+ 0 1) (+ 2 6)]
    [:paper :scissors]    [(+ 0 2) (+ 3 6)]))

(defn my-total-score [lines]
  (->> lines
       (map (comp second score))
       (reduce +)))

;; part 1
(->> (parse-input input)
     (map #(map letter->shape %))
     (my-total-score)) ; 12794

(defn rewrite-line [[a b]]
  (let [shape (letter->shape a)]
    (case b
      ;; X means you need to lose
      "X" (cond
            (= shape :rock)     [shape :scissors]
            (= shape :paper)    [shape :rock]
            (= shape :scissors) [shape :paper])
      ;; Y means you need to end the round in a draw
      "Y" [shape shape]
      ;; and Z means you need to win.
      "Z" (cond
            (= shape :rock)     [shape :paper]
            (= shape :paper)    [shape :scissors]
            (= shape :scissors) [shape :rock]))))

;; part 2
(->> (parse-input input)
     (map rewrite-line)
     (my-total-score)) ; 14979
