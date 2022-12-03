(ns zelark.aoc-2022.day-02
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 2: Rock Paper Scissors ---
;; https://adventofcode.com/2022/day/2

(def input (aoc/get-input 2022 02))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [[a _ b]] [a b]))))

(def letter->shape
  {\A :rock \B :paper \C :scissors
   \X :rock \Y :paper \Z :scissors})

(defn score [state shape]
  (+ (get {:rock 1 :paper 2 :scissors 3} shape)
     (get {:loss 0 :draw 3 :win 6} state)))

(defn state [[a b :as choice]]
  (cond
    (= a b)                       :draw
    (= choice [:scissors :rock])  :win
    (= choice [:rock :paper])     :win
    (= choice [:paper :scissors]) :win
    :else                         :loss))

;; part 1
(defn score-p1 [[_ b :as choice]]
  (score (state choice) b))

(->> (parse-input input)
     (map #(map letter->shape %))
     (transduce (map score-p1) +)) ; 12794

;; part 2
(defn score-p2 [[a b]]
  (let [shape (letter->shape a)]
    (case b
      \X (case shape
           :rock     (score :loss :scissors)
           :paper    (score :loss :rock)
           :scissors (score :loss :paper))
      \Y (score :draw shape)
      \Z (case shape
           :rock     (score :win :paper)
           :paper    (score :win :scissors)
           :scissors (score :win :rock)))))

(->> (parse-input input)
     (transduce (map score-p2) +)) ; 14979
