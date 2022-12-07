(ns zelark.aoc-2022.day-07
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 7: No Space Left On Device ---
;; https://adventofcode.com/2022/day/7

(def input (aoc/get-input 2022 07))

(defn step [[acc path] line]
  (let [[a b c] (str/split line #" ")]
    (cond
      (and (= a "$") (= b "cd"))
      (if (= c "..")
        [acc (pop path)]
        [acc (conj path c)])

      (and (= a "$") (= b "ls"))
      [acc path]

      :else
      (let [file (if (= a "dir")
                   {:name b :path (conj path a)}
                   {:name b :size (parse-long a)})]
        [(update acc path (fnil conj []) file) path]))))

(def directories (->> (str/split-lines input)
                      (reduce step [{} []])
                      (first)))

(defn calc-size [dirs]
  (->> (sort-by (comp count first) > dirs)
       (reduce (fn [acc [path files]]
                 (assoc acc path (->> files
                                      (map (fn [f]
                                             (or (:size f)
                                                 (get acc (:path f)))))
                                      (reduce +))))
               {})))

;; part 1
(->> (calc-size directories)
     (vals)
     (filter #(<= % 100000))
     (reduce +)) ; 1723892

;; part 2
(let [disk-size  70000000
      req-size   30000000
      path->size (calc-size directories)
      root-size  (get path->size ["/"])
      available  (- disk-size root-size)]
  (->> (vals path->size)
       (filter #(<= req-size (+ available %)))
       (sort)
       (first))) ; 8474158
