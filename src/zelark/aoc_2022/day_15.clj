(ns zelark.aoc-2022.day-15
  (:require [clojure.string :as str]
            [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 15: Beacon Exclusion Zone ---
;; https://adventofcode.com/2022/day/15

(def input (aoc/get-input 2022 15))

(defn parse [input]
  (->> (str/split-lines input)
       (reduce (fn [m line]
                 (let [[sx sy bx by] (aoc/parse-longs line)
                       dist (aoc/manhattan-distance [sx sy] [bx by])]
                   (-> m
                       (update :beacons conj  {:x bx, :y by})
                       (update :scanners conj {:x sx, :y sy, :dist dist}))))
               {:beacons #{}, :scanners #{}})))

;; part 1
(let [{:keys [scanners beacons]} (parse input)
      ry 2000000
      [x1 x2] (reduce (fn [[l r :as l+r] {:keys [x y dist]}]
                        (let [gap (abs (- y ry))]
                          (if (<= gap dist)
                            [(min l (- x (- dist gap)))
                             (max r (+ x (- dist gap)))]
                            l+r)))
                      [0 0]
                      scanners)]
  (- (inc (- x2 x1))
     (count (filter #(= (:y %) ry) beacons)))) ; => 5809294

;; part 2: it runs slowly, about 26152 msecs.
(defn scanner-edges [in-bounds? {:keys [x y dist]}]
  (let [delta (inc dist)]
    (->> [[[x (- y delta)] [(+ x delta) y]]
          [[(+ x delta) y] [x (+ y delta)]]
          [[x (+ y delta)] [(- x delta) y]]
          [[(- x delta) y] [x (- y delta)]]]
         (mapcat #(next (apply g2/line-points %)))
         (filter in-bounds?))))

(defn scanned? [scanners position]
  (->> scanners
       (map (fn [{:keys [dist x y]}]
              (< dist (aoc/manhattan-distance [x y] position))))
       (some false?)))

(defn tuning-frequency [[x y]]
  (+ (* x 4000000) y))

(let [{:keys [scanners]} (parse input)
      in-bounds? (partial g2/in-bounds? [[0 0] [4000000 4000000]])]
  (first (for [scanner  (sort-by :dist scanners)
               position (scanner-edges in-bounds? scanner)
               :when    (not (scanned? (disj scanners scanner) position))]
           (tuning-frequency position)))) ; => 10693731308112
