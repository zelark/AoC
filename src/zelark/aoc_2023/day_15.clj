(ns zelark.aoc-2023.day-15
  (:refer-clojure :exclude [hash])
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 15: Lens Library ---
;; https://adventofcode.com/2023/day/15

(def input (aoc/get-input 2023 15))

(defn parse [input]
  (->> (str/split input #",")
       (map #(str/split % #"=|-"))
       (map (fn [[label flen]]
              (if flen [:put label (parse-long flen)] [:del label])))))

(defn hash [s]
  (reduce (fn [hsh ch] (rem (* (+ hsh (int ch)) 17) 256)) 0 s))

;; part 1
(->> (str/split input #",")
     (map hash)
     (aoc/sum)) ; 512283

;; part 2
(defn step [boxes [op label flen]]
  (let [n   (hash label)
        box (boxes n (array-map))]
    (case op
      :put (assoc boxes n (assoc box label flen))
      :del (assoc boxes n (dissoc box label)))))

(defn focusing-power [boxes]
  (reduce-kv (fn [acc n box]
               (->> (map-indexed (fn [i [_ flen]] (* (inc n) (inc i) flen)) box)
                    (into acc)))
             [] boxes))

;; NB: We can use just array-map for a box. It preserves the order.
;; It will work as there are not many lens to put in a single box.
(->> (parse input)
     (reduce step {})
     (focusing-power)
     (aoc/sum)) ; 215827
