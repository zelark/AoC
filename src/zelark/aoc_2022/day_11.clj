(ns zelark.aoc-2022.day-11
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 11: Monkey in the Middle ---
;; https://adventofcode.com/2022/day/11

(def input (aoc/get-input 2022 11))

(defn parse-operation [s]
  (let [[_ _ x op y] (re-seq #"\w+|\+|\*" s)
        op (resolve (symbol op))]
    (if (= x y)
      (fn [x] (op x x))
      (fn [x] (op x (parse-long y))))))

(defn parse-monkey [s]
  (let [[_ items op test mt mf] (str/split-lines s)
        [div m1 m2] (aoc/parse-longs (str test mt mf))]
    {:items (aoc/parse-longs items)
     :div   div
     :op    (parse-operation op)
     :test  #(if (zero? (mod % div)) m1 m2)}))

(defn parse-input [input]
  (->> (aoc/split-on-blankline input)
       (mapv parse-monkey)))

(defn round [reduce-worry init-state]
  (let [turn (fn [state current]
               (let [{:keys [items op test]} (nth state current)] ; Current monkey's stuff.
                 (-> (reduce (fn [state item]
                               (let [item' (reduce-worry (op item))
                                     to    (test item')]
                                 (update-in state [to :items] conj item'))) ; Throw item to monkey `to`.
                             state
                             items)
                     (assoc-in  [current :items] [])
                     (update-in [current :inspected] (fnil + 0) (count items)))))]
    (reduce turn init-state (range (count init-state)))))

(defn solve
  "Calculatates the level of monkey business."
  [reduce-worry n input]
  (->> (parse-input input)
       (iterate (partial round reduce-worry))
       (drop n) ; Play n rounds.
       (first)  
       (map :inspected)
       (sort >)
       (take 2) ; Two most active monkeys.
       (apply *)))

;; part 1
(solve #(quot % 3) 20 input) ; 98280

;; part 2
(let [lcm (->> (parse-input input) (map :div) (reduce *))]
  (solve #(mod % lcm) 10000 input)) ; 17673687232

