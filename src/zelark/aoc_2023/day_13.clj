(ns zelark.aoc-2023.day-13
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 13: Point of Incidence ---
;; https://adventofcode.com/2023/day/13

(def input (aoc/get-input 2023 13))

(defn parse [input]
  (->> (aoc/split-on-blankline input)
       (map (fn [pattern] (map vec (str/split-lines pattern))))))

(defn reflection? [pattern i j]
  (let [a (nth pattern i nil)
        b (nth pattern j nil)]
    (or (not a)
        (not b)
        (and (= a b)
             (reflection? pattern (dec i) (inc j))))))

(defn find-reflection [pattern k]
  (when-let [found (->> (partition 2 1 pattern)
                        (reduce (fn [state [a b]]
                                  (if (= a b)
                                    (-> state
                                        (update :n inc)
                                        (update :found conj (:n state)))
                                    (update state :n inc)))
                                {:n 0 :found []})
                        (:found)
                        (seq))]
    (when-let [found (some #(when (reflection? pattern % (inc %)) %) found)]
      [k (inc found)])))

(defn summarize [cols rows]
  (+ cols (* rows 100)))

(defn solve [f input]
  (->> (parse input)
       (map (fn [p]
              (or (f p :rows)
                  (f (aoc/transpose p) :cols))))
       (reduce (fn [[c r] [k n]]
                 (case k
                   :cols [(+ c n) r]
                   :rows [c (+ r n)]))
               [0 0])
       (apply summarize)))


;; part 1
(solve find-reflection input) ; 27300

;; part 2
(defn diff-by-one? [a b]
  (and (== (count a) (count b))
       (->> (map compare a b)
            (reduce (fn [d n] (if (not (zero? n)) (inc d) d)) 0)
            (contains? #{1}))))

(defn reflection?-2 [pattern i j smudge]
  (let [a (nth pattern i nil)
        b (nth pattern j nil)]
    (or (and (not a) (aoc/one? smudge))
        (and (not b) (aoc/one? smudge))
        (and (= a b)
             (reflection?-2 pattern (dec i) (inc j) smudge))
        (and (diff-by-one? a b)
             (zero? smudge)
             (reflection?-2 pattern (dec i) (inc j) 1)))))

(defn find-reflection-2 [pattern k]
  (when-let [found (->> (partition 2 1 pattern)
                        (reduce (fn [state [a b]]
                                  (if (or (= a b)
                                          (diff-by-one? a b))
                                    (-> state
                                        (update :n inc)
                                        (update :found conj (:n state)))
                                    (update state :n inc)))
                                {:n 0 :found []})
                        (:found)
                        (seq))]
    (when-let [found (some #(when (reflection?-2 pattern % (inc %) 0) %) found)]
      [k (inc found)])))

(solve find-reflection-2 input) ; 29276
