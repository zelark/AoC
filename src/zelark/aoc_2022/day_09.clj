(ns zelark.aoc-2022.day-09
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 9: Rope Bridge ---
;; https://adventofcode.com/2022/day/9

(def input (aoc/get-input 2022 9))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[c n] (str/split line #" ")]
                [(keyword c) (parse-long n)])))))

(def dirs {:U [0 -1] :R [1 0] :D [0 1] :L [-1 0]})

(defn sub [a b] (mapv - a b))
(defn add [a b] (mapv + a b))

(def adjacent? (set (for [x [-1 0 1], y [-1 0 1]] [x y])))

(defn near? [a b] (adjacent? (sub a b)))

(defn delta [x]
  (if (zero? x) x (if (neg? x) -1 1)))

;; [# # # # #]
;; [# . . . #]
;; [# . # . #]
;; [# . . . #]
;; [# # # # #]

(defn direction [a b]
  (let [[dx dy] (sub a b)]
    [(delta dx) (delta dy)]))

(defn step [a b]
  (if (near? a b)
    b
    (add b (direction a b))))

(defn move [state cmd]
  (let [[dir n] cmd]
    (loop [state state
           n n]
      (if (zero? n)
        state
        (recur (let [{:keys [body]} state
                     head  (first body)
                     head' (add head (get dirs dir))
                     body' (reduce (fn [acc part]
                                     (conj acc (step (peek acc) part)))
                                   [head']
                                   (rest body))]
                 (-> state
                     (assoc :body body')
                     (update :steps conj (peek body'))))
               (dec n))))))

(defn solve [input n]
  (->> (parse-input input)
       (reduce move {:body (repeat n [0 0])
                     :steps #{}})
       :steps
       (count)))

;; part 1
(solve input 2)  ; 6391

;; part 2
(solve input 10) ; 2593
