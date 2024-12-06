(ns zelark.aoc-2024.day-06
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 6: Guard Gallivant ---
;; https://adventofcode.com/2024/day/6

(def input (aoc/get-input 2024 6))

(def directions {\^ g2/up \v g2/down \> g2/right \< g2/left})

(defn find-guard [grid]
  (some (fn [[loc ch]] (when-let [d (directions ch)] [loc d])) grid))

(defn small-step [grid [pos dir]]
  (let [new-pos (g2/plus pos dir)]
    (when-let [tile (grid new-pos)]
      (if (g2/empty-space? tile)
        [new-pos dir]
        (recur grid [pos (g2/turn :right dir)])))))

(defn big-step [grid [pos dir]]
  (let [new-pos (g2/plus pos dir)]
    (when-let [tile (grid new-pos)]
      (if (g2/empty-space? tile)
        (recur grid [new-pos dir])
        [pos (g2/turn :right dir)]))))

(defn guard-path [step grid pos+dir]
  (->> (iterate #(step grid %) pos+dir)
       (take-while some?)))

;; part 1 (23.453246 msecs)
(time (let [grid        (g2/parse input)
            [start dir] (find-guard grid)]
        (->> (guard-path small-step (assoc grid start g2/empty-space) [start dir])
             (into #{} (map first))
             (count)))) ; 5242

(defn debug [grid path]
  (-> (reduce (fn [m [pos]] (assoc m pos \x)) grid path)
      (aoc/print-points-2)))

;; part 2 (6360.225645 msecs)
(defn in-a-loop? [grid [start dir] block-position]
  (let [grid' (assoc grid block-position \O)]
    (->> (guard-path big-step grid' [start dir])
         (drop 1)
         #_(reduce #(if (%1 %2) (reduced true) (conj! %1 %2)) (transient #{}))
         (reduce #(if (%1 %2) (reduced true) (conj %1 %2)) #{})
         (true?))))

;; 16071.819368 msecs
;; 7152.590738 msecs
(time (let [grid        (g2/parse input)
            [start dir] (find-guard grid)
            grid        (assoc grid start g2/empty-space)
            positions   (->> (guard-path small-step grid [start dir])
                             (into #{} (map first)))]
        (->> (map #(in-a-loop? grid [start dir] %) (disj positions start))
             (filter true?)
             (count)))) ; 1424

;; 16071.819368 msecs
;; 4646.135041 msecs
;; 3645.085527 msecs
;; 1954.627387 msecs
(time (let [grid        (g2/parse input)
            [start dir] (find-guard grid)
            grid        (assoc grid start g2/empty-space)
            path        (->> (guard-path small-step grid [start dir])
                             (reduce (fn [[acc seen] [pos dir]]
                                       (if (seen pos)
                                         [acc seen]
                                         [(conj acc [pos dir]) (conj seen pos)]))
                                     [[] #{}])
                             (first))]
        (->> (rest path)
             (pmap (fn [[pos dir]] (in-a-loop? grid [(g2/minus pos dir) dir] pos)))
             (filter true?)
             (count))))
