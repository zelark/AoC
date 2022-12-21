(ns zelark.aoc-2022.day-21
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 21: Monkey Math ---
;; https://adventofcode.com/2022/day/21

(def input (aoc/get-input 2022 21))

(defn parse-monkey-job [m monkey-job]
  (let [[monkey a op b] (->> (str/replace monkey-job ":" "")
                             (format "[%s]")
                             (edn/read-string))]
    (if (number? a)
      (assoc m (keyword monkey) {:number a})
      (assoc m (keyword monkey) {:op (resolve op)
                                 :a  (keyword a)
                                 :b  (keyword b)}))))

(defn parse [input]
  (->> (str/split-lines input)
       (reduce parse-monkey-job {})))

(defn yell [monkey->job monkey]
  (when-let [{:keys [number op a b]} (monkey->job monkey)]
    (if number
      number
      (op (yell monkey->job a)
          (yell monkey->job b)))))

;; part 1
(yell (parse input) :root) ; => 379578518396784

;; part 2: It turned out we can find the number with extrapolation.
(let [monkey->job (-> (parse input)
                      (assoc-in [:root :op] -))
      y0 (yell (assoc-in monkey->job [:humn :number] 0) :root)
      y1 (yell (assoc-in monkey->job [:humn :number] 1) :root)
      n  (/ y0 (- y0 y1))]
  (when (zero? (yell (assoc-in monkey->job [:humn :number] n) :root))
    (long n))) ; => 3353687996514
