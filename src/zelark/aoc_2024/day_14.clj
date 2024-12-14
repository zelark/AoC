(ns zelark.aoc-2024.day-14
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 14: Restroom Redoubt ---
;; https://adventofcode.com/2024/day/14

(def input (aoc/get-input 2024 14))

(def ^:const width 101)
(def ^:const height 103)

(defn parse-input [input]
  (->> (str/split-lines input)
       (map aoc/parse-longs)))

(defn move-robot [[x y vx vy]]
  (let [x' (mod (+ x vx) width)
        y' (mod (+ y vy) height)]
    [x' y' vx vy]))

(defn step [robots]
  (doall (map move-robot robots)))

(defn quadrant [[x y]]
  (let [mx (quot width 2)
        my (quot height 2)]
    (if (or (= x mx) (= y my))
      :middle
      [(quot x (inc mx))
       (quot y (inc my))])))

(defn safety-factor [robots]
  (let [quadrants (-> (group-by quadrant robots)
                      (dissoc :middle))]
    (aoc/mul (comp count val) quadrants)))

;; part 1 (6.547224 msecs)
(->> (parse-input input)
     (iterate step)
     (drop 100)
     (first)
     (safety-factor)) ; 229980828

;; part 2 (621.417039 msecs)
(defn easter-egg?
  "Returns true when robots do not overlap with each other."
  [robots]
  (boolean (reduce (fn [seen [x y]]
                     (if (seen [x y]) (reduced false) (conj seen [x y])))
                   #{}
                   robots)))

(->> (parse-input input)
     (iterate step)
     (take-while (complement easter-egg?))
     (count)) ; 7132

;; Another approach — slower but more reliable.
;; It takes into account a safety factor.
(->> (parse-input input)
     (iterate step)
     (map-indexed vector)
     (reduce (fn [seen [idx state]]
               (if (seen state)
                 (reduced seen)
                 (assoc seen state idx)))
             {})
     (apply min-key (comp safety-factor key))
     (val))

;; bonus: draw a picture of the Christmas tree
(->> (parse-input input)
     (iterate step)
     (drop-while (complement easter-egg?))
     (first)
     (map (juxt first second))
     (aoc/print-points))
