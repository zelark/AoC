(ns zelark.aoc-2023.day-05
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.range :as r]
            [clojure.string :as str]))

;; --- Day 5: If You Give A Seed A Fertilizer ---
;; https://adventofcode.com/2023/day/5

(def input (aoc/get-input 2023 05))

(defn parse-map [m]
  (->> (str/split-lines m)
       (drop 1) ; name of the map
       (mapv #(let [[dst src len] (aoc/parse-longs %)]
                {:source (r/rangel src len)
                 :offset (- dst src)}))))

(defn parse [input]
  (let [[seeds & maps] (aoc/split-on-blankline input)]
    {:seeds (aoc/parse-longs seeds)
     :maps  (map parse-map maps)}))

(defn convert [mapping range]
  (into (apply r/exclude range (map :source mapping))
        (keep (fn [{:keys [source offset]}]
                (some-> (r/intersection source range)
                        (r/shift offset)))
              mapping)))

(defn lowest-location-number [maps seeds]
  (let [f (fn [ranges mapping] (mapcat #(convert mapping %) ranges))]
    (->> (reduce f seeds maps)
         (apply min-key :start)
         (:start))))

;; part 1 (7.148303 msecs)
(let [{:keys [seeds maps]} (parse input)]
  (->> (map #(r/rangel % 1) seeds)
       (lowest-location-number maps))) ; 199602917

;; part 2 (9.382353 msecs)
(let [{:keys [seeds maps]} (parse input)]
  (->> (partition 2 seeds)
       (map (fn [[start len]] (r/rangel start len)))
       (lowest-location-number maps))) ; 2254686
