(ns zelark.aoc-2022.day-25
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 25: Full of Hot Air ---
;; https://adventofcode.com/2022/day/25

(def input (aoc/get-input 2022 25))

(defn parse [input]
  (str/split-lines input))

(defn snafu->dec [snafu]
  (->> (map {\2 2 \1 1 \0 0 \- -1 \= -2} snafu)
       (reduce (fn [n d] (+ (* n 5) d)) 0)))

(defn dec->snafu [decimal]
  (loop [n   decimal
         ret ()]
    (if (zero? n)
      (str/join ret)
      (let [[n r] ((juxt quot rem) n 5)]
        (recur (+ n ({3 1 4 1} r 0))
               (cons ({3 \= 4 \-} r r) ret))))))

;; part 1
(->> (parse input)
     (map snafu->dec)
     (aoc/sum)
     (dec->snafu)) ; => 2=-0=01----22-0-1-10

;; part 2
;; With all 50 stars we can start the blender, and make a smoothie
;; to the reindeer.

;; Just for fun here is SNAFU addition table:
;;
;;             =  -  0  1  2
;;          = -1 -2  =  -  0
;;          - -2  =  -  0  1
;;          0  =  -  0  1  2
;;          1  -  0  1  2 1=
;;          2  0  1  2 1= 1-
;;
