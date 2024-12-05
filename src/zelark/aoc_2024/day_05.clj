(ns zelark.aoc-2024.day-05
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 5: Print Queue ---
;; https://adventofcode.com/2024/day/5

(def input (aoc/get-input 2024 5))

(defn parse-rules [rules]
  (->> (str/split-lines rules)
       (mapv aoc/parse-longs)
       (reduce (fn [m [a b]]
                 (update m a (fnil conj #{}) b))
               {})))

(defn parse-input [input]
  (let [[rules updates] (aoc/split-on-blankline input)]
    {:rules   (parse-rules rules)
     :updates (->> (str/split-lines updates)
                   (mapv aoc/parse-longs))}))

;; part 1 (6.065864 msecs)
(defn correct-order? [rules [current & left-pages]]
  (cond
    (empty? left-pages) true

    (some #((get rules % #{}) current) left-pages)
    false

    :else (recur rules left-pages)))

(defn sum-middle-page-numbers [updates]
  (let [middle #(quot (count %) 2)]
    (aoc/sum #(nth % (middle %)) updates)))

(let [{:keys [rules updates]} (parse-input input)]
  (->> (filter #(correct-order? rules %) updates)
       (sum-middle-page-numbers))) ; 6949

;; part 2 (6.637427 msecs)
(defn cmp [rules]
  (fn [a b]
    (cond
      ((rules a #{}) b) -1
      ((rules b #{}) a)  1
      :else              0)))

(defn fix-order [rules pages]
  (sort (cmp rules) pages))

(let [{:keys [rules updates]} (parse-input input)]
  (->> (remove #(correct-order? rules %) updates)
       (map #(fix-order rules %))
       (sum-middle-page-numbers))) ; 4145
 