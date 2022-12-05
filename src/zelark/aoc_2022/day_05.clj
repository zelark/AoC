(ns zelark.aoc-2022.day-05
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 5: Supply Stacks ---
;; https://adventofcode.com/2022/day/5

(def input (aoc/get-input 2022 05))

(defn parse-line [line]
  (->> (re-seq #".{3}\s?" line)
       (map #(re-find #"[A-Z]" %))))

(defn parse-input [input]
  (let [[stacks procedures] (aoc/split-on-blankline input)
        stacks (->> (str/split-lines stacks)
                    (butlast)
                    (reverse)
                    (map parse-line)
                    (aoc/transpose)
                    (mapv #(vec (remove str/blank? %))))]
    {:stacks stacks
     :procedures (->> (str/split-lines procedures)
                      (map aoc/parse-longs)
                      (map #(-> % (update 1 dec) (update 2 dec))))}))

(defn move-by-one [stacks proc]
  (let [[n from to] proc
        stack-from (get stacks from)
        stack-to   (get stacks to)]
    (loop [src stack-from
           dst stack-to
           n n]
      (if (zero? n)
        (assoc stacks from src to dst)
        (recur (pop src)
               (conj dst (peek src))
               (dec n))))))

(defn move-by-pile [stacks proc]
  (let [[n from to] proc
        src (get stacks from)
        dst (get stacks to)
        len (count src)
        pile (subvec src (- len n) len)]
    (assoc stacks from (subvec src 0 (- len n)) to (into dst pile))))

(defn solve [move-fn input]
  (let [{:keys [stacks procedures]} (parse-input input)]
    (->> (reduce #(move-fn %1 %2) stacks procedures)
         (map peek)
         (apply str))))

;; part 1
(solve move-by-one input) ; "TPGVQPFDH"

;; part 2
(solve move-by-pile input) ; "DMRDFRHHH"
