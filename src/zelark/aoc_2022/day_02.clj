(ns zelark.aoc-2022.day-02
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 2: Rock Paper Scissors ---
;; https://adventofcode.com/2022/day/2

(def input (aoc/get-input 2022 02))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [[a _ b]] [a b]))))

(def letter->shape
  {\A :rock \B :paper \C :scissors
   \X :rock \Y :paper \Z :scissors})

(def lose {:rock     :scissors
           :paper    :rock
           :scissors :paper})

(def win (set/map-invert lose))

(defn score [outcome shape]
  (+ (get {:rock 1 :paper 2 :scissors 3} shape)
     (get {:loss 0 :draw 3 :win 6} outcome)))

(defn outcome [a b]
  (cond
    (= a b)                      :draw
    (= [a b] [:rock :scissors])  :win
    (= [a b] [:paper :rock])     :win
    (= [a b] [:scissors :paper]) :win
    :else                        :loss))

;; part 1
(defn score-p1 [[a b]]
  (score (outcome b a) b))

(->> (parse-input input)
     (map #(map letter->shape %))
     (transduce (map score-p1) +)) ; 12794

;; part 2
(defn score-p2 [[a b]]
  (let [shape (letter->shape a)]
    (case b
      \X (score :loss (lose shape))
      \Y (score :draw shape)
      \Z (score :win (win shape)))))

(->> (parse-input input)
     (transduce (map score-p2) +)) ; 14979
