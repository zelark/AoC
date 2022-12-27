(ns zelark.aoc-2018.day-02
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [zelark.aoc.core :as aoc]))

;; --- Day 2: Inventory Management System ---
;; https://adventofcode.com/2018/day/02

(def input (aoc/get-input 2018 02))

(defn parse [input]
  (str/split-lines input))

;; part 1
(let [counts (->> (parse input)
                  (map #(-> % frequencies vals set)))]
  (* (count (filter #(% 2) counts))
     (count (filter #(% 3) counts)))) ; => 8118

;; part 2
(defn differ-by-one? [a b]
  (->> (map compare a b) (remove zero?) count aoc/one?))

(defn common-letters [a b]
  (->> (map vector a b)
       (reduce (fn [s [a b]] (if (= a b) (str s a) s)) "")))

;; A naive slow solution. For the given input it runs ~144 msecs.
(let [ids (parse input)]
  (->> (combo/combinations ids 2)
       (some (fn [[a b]]
               (when (differ-by-one? a b)
                 (common-letters a b)))))) ; => "jbbenqtlaxhivmwyscjukztdp"

;; We can do it faster by using a hash-set. Runs ~7.2 msecs
(let [ids (parse input)]
  (time (loop [seen #{}, ids ids]
          (when-let [id (first ids)]
            (let [ret (reduce (fn [seen i]
                                (let [s (str (subs id 0 i) "*" (subs id (inc i)))]
                                  (if (seen s) (reduced s) (conj seen s))))
                              seen
                              (range (count id)))]
              (if (set? ret)
                (recur ret (next ids))
                (str/replace-first ret \* ""))))))) ; => "jbbenqtlaxhivmwyscjukztdp"
