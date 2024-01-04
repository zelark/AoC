(ns zelark.aoc-2023.day-12
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 12: Hot Springs ---
;; https://adventofcode.com/2023/day/12

(def input (aoc/get-input 2023 12))

(defn parse-row [row]
  (let [[springs sizes] (str/split row #"\s")]
    [springs (aoc/parse-longs sizes)]))

(defn parse [input]
  (->> (str/split-lines input)
       (mapv parse-row)))

;; part 1
(defn operational? [^Character ch] (= ch \.))
(defn demaged? [^Character ch] (= ch \#))

(defn all-demaged? [^String row]
  (not (str/index-of row \.)))

(defn count-arrangements [[records sizes]]
  (let [cache (java.util.HashMap.)
        fit? (fn [start len]
               (and (<= 0 start)
                    (<= (+ start len) (count records))
                    (all-demaged? (subs records start (+ start len)))
                    (or (zero? start) ; Should be a space for the separator.
                        (not (demaged? (nth records (dec start)))))))
        ; Counts arragements of springs for i records and j sizes.
        cnt (fn cnt [i j branch?]
              (let [key (+ (* 10000 i) j)] ; To bost hashing and comparison.
                (or (and (not branch?) (.get cache key))
                    (let [n (cond
                              (and (zero? i) (pos? j))  0
                              (and (zero? i) (zero? j)) 1

                              (and (not branch?)
                                   (not (demaged? (nth records (dec i)))))
                              (+ (cnt i j true) ; Here is a branch!
                                 (cnt (dec i) j false))

                              (and (pos? j) ; There are more sizes.
                                   (not (operational? (nth records (dec i))))
                                   (fit? (- i (sizes (dec j))) (sizes (dec j))))
                              (let [size (sizes (dec j))
                                    sep  (if (== i size) 0 1)]       ; Groups are always separated by at least
                                (cnt (- i size sep) (dec j) false))  ; one operational spring.

                              :else 0)
                          _ (.put cache key n)]
                      n))))]
    (cnt (count records) (count sizes) false)))

(comment
  (count-arrangements ["?###????????" [3 2 1]]) ; => 10
  (count-arrangements ["????.######..#####." [1 6 5]])) ; => 4

;; (14.218 msecs)
(->> (parse input)
     (aoc/sum count-arrangements)) ; 7599

;; part 2 (138.065 msecs)
(defn unfold-records [n [records sizes]]
  [(str/join "?" (repeat n records))
   (vec (flatten (repeat n sizes)))])

(->> (parse input)
     (map (partial unfold-records 5))
     (aoc/sum count-arrangements)) ; 15454556629917

;; Other interesting implementations:
;; https://gist.github.com/alexander-yakushev/7f40fbafa4fa29b93706ca823be4250a
;; https://www.reddit.com/r/adventofcode/comments/18gomx5/2023_day_12_without_memoization_caches_or/
