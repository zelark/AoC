(ns zelark.aoc-2024.day-25
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 25: Code Chronicle ---
;; https://adventofcode.com/2024/day/25

(def input (aoc/get-input 2024 25))

(defn parse-item [lines]
  (let [item (str/split-lines lines)]
    (if (= (first item) "#####")
      [:locks (mapv #(aoc/cnt % \#) (aoc/transpose (rest item)))]
      [:keys  (mapv #(aoc/cnt % \#) (aoc/rotate-cw (butlast item)))])))

(defn parse-input [input]
  (->> (aoc/split-on-blankline input)
       (map parse-item)
       (reduce (fn [m [k v]] (update m k conj v)) {:locks [] :keys []})))

(defn fit? [lock key]
  (->> (map + lock key)
       (every? #{0 1 2 3 4 5})))

;; part 1 (44.668974 msecs)
(let [{:keys [keys locks]} (parse-input input)]
  (-> (for [lock  locks
            key   keys
            :when (fit? lock key)]
        1)
      (aoc/sum)))
