(ns zelark.aoc-2023.day-03
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]
            [clojure.string :as str]))

;; --- Day 3: Gear Ratios ---
;; https://adventofcode.com/2023/day/3

(def input (aoc/get-input 2023 03))

(defn adjecent [dx y]
  (->> (for [x (apply range dx)] [x y])
       (reduce (fn [acc loc]
                 (into acc (g2/all-neighbors loc)))
               #{})))

(defn parse-line [line y]
  (let [matcher (re-matcher #"\d+|[^\.]" line)]
    (loop [match (re-find matcher)
           res   []]
      (if match
        (let [x1 (.start matcher)
              x2 (.end matcher)
              match (or (parse-long match) match)]
          (recur (re-find matcher)
                 (if (number? match)
                   (conj res [[x1 y] {:number match
                                      :adjecent (adjecent [x1 x2] y)}])
                   (conj res [[x1 y] {:symbol match
                                      :part-numbers []}]))))
        res))))

(defn add-part-numbers [m {:keys [number adjecent]}]
  (reduce (fn [m2 loc]
            (if (get-in m2 [loc :symbol])
              (update-in m2 [loc :part-numbers] conj number)
              m2))
          m
          adjecent))

(defn parse-input [input]
  (let [g (->> (str/split-lines input)
               (mapcat (fn [i line] (parse-line line i)) (range))
               (reduce (fn [m [k v]] (assoc m k v)) {}))]
    (vals (reduce-kv (fn [m k v]
                       (if (:number v)
                         (-> m (add-part-numbers v) (dissoc k))
                         m))
                     g
                     g))))

;; part 1
(->> (parse-input input)
     (aoc/sum #(-> % :part-numbers aoc/sum))) ; 512794

;; part 2
(->> (parse-input input)
     (filter (fn [{:keys [symbol part-numbers]}]
               (and (= symbol "*")
                    (aoc/len part-numbers = 2))))
     (aoc/sum #(-> % :part-numbers aoc/mul))) ; 67779080
