(ns zelark.aoc-2023.day-19
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.range :as r]
            [clojure.string :as str]
            [clojure.edn :as edn]))

;; --- Day 19: Aplenty ---
;; https://adventofcode.com/2023/day/19

(def input (aoc/get-input 2023 19))

(defn ->part [s]
  (-> (str/replace s #"=" " ")
      (edn/read-string)
      (update-keys keyword)))

(defn ->rule [s]
  (if (str/includes? s ":")
    (let [[_ rating op n workflow] (re-find #"([xmas])(<|>)(\d+):([a-zAR]+)" s)]
      {:rule [(case op "<" < ">" >) (keyword rating) (parse-long n)]
       :to   workflow})
    {:to s}))

(defn ->workflow [s]
  (let [[_ workflow rules] (re-find #"([a-z]+)\{(.+)\}" s)
        rules (mapv ->rule (str/split rules #","))]
    [workflow rules]))

(defn parse [input]
  (let [[workflows parts] (aoc/split-on-blankline input)]
    {:workflows (->> (map ->workflow (str/split-lines workflows))
                     (reduce (fn [m [name rules]] (assoc m name rules)) {}))
     :parts     (map ->part (str/split-lines parts))}))

;; part 1
(defn apply-rules [rules part]
  (some (fn [{:keys [to rule]}]
          (if-let [[op rating n] rule]
            (and (op (part rating) n) to)
            to))
        rules))

(defn accepted? [workflows part]
  (loop [rules (workflows "in")]
    (let [next-workflow (apply-rules rules part)]
      (case next-workflow
        "A" true
        "R" false
        (recur (workflows next-workflow))))))

(let [{:keys [workflows parts]} (parse input)]
  (->> (filter #(accepted? workflows %) parts)
       (reduce (fn [acc {:keys [x m a s]}] (+ acc x m a s)) 0))) ; 319295

;; part 2
(defn walk [workflows comb]
  (reduce (fn [[acc cmb] {:keys [to rule]}]
            (if-let [[op r n] rule]
              (condp = op
                < (let [[passed failed] (r/split (cmb r) n)]
                    [(conj acc (assoc cmb r passed, :wf to))
                     (assoc cmb r failed)])
                > (let [[failed passed] (r/split (cmb r) (inc n))]
                    [(conj acc (assoc cmb r passed, :wf to))
                     (assoc cmb r failed)]))
              (conj acc (assoc cmb :wf to))))
          [[] comb]
          (workflows (comb :wf))))

(let [{:keys [workflows]} (parse input)
      rr    (r/rangel 1 4000)
      combs [{:wf "in" :x rr :m rr :a rr :s rr}]]
  (->> (loop [combs combs
              res   []]
         (if (seq combs)
           (let [{:keys [accepted misc]}
                 (->> (map #(walk workflows %) combs)
                      (flatten)
                      (group-by #(case (% :wf) "A" :accepted "R" :rejected :misc)))]
             (recur misc (into res accepted)))
           res))
       (map (fn [{:keys [x m a s]}] (aoc/mul r/length [x m a s])))
       (aoc/sum))) ; 110807725108076
