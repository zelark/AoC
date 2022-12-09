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

(defn delta [a]
  (if (zero? a) a (if (neg? a) -1 1)))

;; [# # # # #]
;; [# . . . #]
;; [# . # . #]
;; [# . . . #]
;; [# # # # #]

(defn direction [a b]
  (let [[dx dy] (sub a b)]
    [(delta dx) (delta dy)]))

(defn follow [a b]
  (if (near? a b)
    b
    (add b (direction a b))))

(defn move-rope [state cmd]
  (let [[dir n] cmd]
    (loop [state state
           n n]
      (if (zero? n)
        state
        (recur (let [{:keys [rope]} state
                     head  (first rope)
                     head' (add head (get dirs dir))
                     rope' (reduce (fn [a b]
                                     (conj a (follow (peek a) b)))
                                   [head']
                                   (rest rope))]
                 (-> state
                     (assoc :rope rope')
                     (update :steps conj (peek rope'))))
               (dec n))))))

(defn solve [input n]
  (->> (parse-input input)
       (reduce move-rope {:rope (repeat n [0 0])
                          :steps #{}})
       :steps
       (count)))

;; part 1
(solve input 2)  ; 6391

;; part 2
(solve input 10) ; 2593
