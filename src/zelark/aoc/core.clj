(ns zelark.aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as shell]))

(defn get-input [year day]
  (let [path-to-file (format "%d/input_%02d.txt" year day)
        load-input #(-> % io/resource slurp str/trim-newline)]
    (if (.exists (io/file path-to-file))
      (load-input path-to-file)
      (do (shell/sh "./bin/fetch-input" (str year) (str day))
          (load-input path-to-file)))))

;; Parsing
(defn parse-longs [s]
  (->> (re-seq #"-?\d+" s)
       (mapv parse-long)))

(defn parse-bin [s]
  (Long/parseLong s 2))

(defn split-on-blankline [input]
  (str/split input #"\R\R"))

;; Math
(defn mod-1
  "Returns the 1-based modulus `base` of `n`"
  [n base]
  (inc (mod (dec n) base)))

(defn rangex
  ([] ())
  ([start end]
   (if (<= start end)
     (range start (inc end))
     (range start (dec end) -1))))

(defn sum [xs]
  (reduce + 0 xs))

(defn fix-point [f x]
  (let [x' (f x)]
    (if (= x x') x (recur f x'))))

;; Grids
(defn empty-grid [w h]
  (vec (repeat h (vec (repeat w \.)))))

(defn print-grid [grid]
  (->> (map str/join grid)
       (str/join \newline)
       (println)))

(defn mark-point [grid [x y] v]
  (assoc-in grid [y x] v))

(defn print-points [points]
  (let [max-x (apply max (map first points))
        max-y (apply max (map second points))]
    (-> (reduce #(mark-point %1 %2 \#)
                (empty-grid (inc max-x) (inc max-y))
                points)
        (print-grid))))
