(ns zelark.aoc-2022.day-23
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 23: Unstable Diffusion ---
;; https://adventofcode.com/2022/day/23

(def input (aoc/get-input 2022 23))

(defn parse [input]
  (->> (g2/parse input #{\#}) keys set))

(def directions
  (->> (for [[dx dy] [[-1 -1] [0 -1] [+1 -1]   ; NW N NE
                      [-1 +1] [0 +1] [+1 +1]   ; SW S SE
                      [-1 -1] [-1 0] [-1 +1]   ; NW W SW
                      [+1 -1] [+1 0] [+1 +1]]] ; NE E SE 
         [dx dy])
       (partition 3)))

(defn check-dir [elves elf dir]
  (let [adjacent (map #(mapv + %1 %2) (repeat elf) dir)]
    (when-not (some elves adjacent)
      (mapv + elf (second dir)))))

(defn make-proposals [elves dirs]
  (reduce (fn [m elf]
            (if (some elves (g2/all-neighbors elf))
              (if-let [propose (some #(check-dir elves elf %) dirs)]
                (assoc m elf propose)
                (assoc m elf elf))
              (assoc m elf elf)))
          {}
          elves))

(defn step [elves dirs]
  (let [elf->propose (make-proposals elves dirs)
        clashes (frequencies (vals elf->propose))
        clash? #(<= 2 (clashes %))]
    (reduce-kv (fn [elves elf prop]
                 (conj elves (if (clash? prop) elf prop)))
               #{}
               elf->propose)))

(defn count-empty-titels [elves]
  (let [[[x1 y1] [x2 y2]] (g2/boundaries elves)]
    (- (* (inc (- x2 x1))
          (inc (- y2 y1)))
       (count elves))))

(defn solve [pred input]
  (loop [elves      (parse input)
         directions (cycle directions)
         round      0]
    (let [elves' (step elves (take 4 directions))]
      (if (pred elves elves' round)
        [elves (inc round)]
        (recur elves' (drop 1 directions) (inc round))))))

;; part 1
(-> (solve (fn [_ _ round] (== round 10)) input)
    (first)
    (count-empty-titels)) ; => 4091

;; part 2
(-> (solve (fn [old new _] (= old new)) input)
    (second)) ; => 1036
