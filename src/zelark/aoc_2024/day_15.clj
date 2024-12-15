(ns zelark.aoc-2024.day-15
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [zelark.aoc.graph :as g]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 15: Warehouse Woes ---
;; https://adventofcode.com/2024/day/15

(def input (aoc/get-input 2024 15))

(def directions {\^ g2/up \v g2/down \> g2/right \< g2/left})

(def scale-up #(str/escape % {\# "##" \. ".." \@ "@." \O "[]"}))

(defn parse-input [input & {:keys [scale?]}]
  (let [[warehouse movements] (aoc/split-on-blankline input)
        grid  (cond-> warehouse
                scale?  (scale-up)
                :always (g2/parse))
        robot (some #(when (= (val %) \@) (key %)) grid)]
    {:warehouse (assoc grid robot g2/empty-space)
     :robot     robot
     :movements (map first (re-seq #"[\^v><]" movements))}))

(defn sum-of-boxes [{:keys [wh]}]
  (aoc/sum (fn [[[x y] tile]]
             (if (#{\O \[} tile) (+ (* 100 y) x) 0))
           wh))

(defn movable? [wh locations]
  (not-any? #{\#} (map wh locations)))

(defn move-boxes [wh locations move]
  (let [cleaned (reduce #(assoc %1 %2 g2/empty-space) wh locations)]
    (reduce (fn [w loc] (assoc w (g2/plus loc move) (wh loc)))
            cleaned
            locations)))

(defn step [{:keys [wh robot] :as state} move]
  (let [vertical? #{g2/up g2/down}
        neighbors (fn [loc]
                    (case (wh loc)
                      \[ (cond-> [(g2/plus loc move)] (vertical? move) (conj (g2/plus loc g2/right)))
                      \] (cond-> [(g2/plus loc move)] (vertical? move) (conj (g2/plus loc g2/left)))
                      \O [(g2/plus loc move)]
                      []))
        robot'    (g2/plus robot move)
        locations (->> (g/connected-group neighbors robot')
                       (remove (comp g2/empty-space? wh)))]
    (if (movable? wh locations)
      {:wh    (cond-> wh (seq locations) (move-boxes locations move))
       :robot robot'}
      state)))

(defn solve [part input]
  (let [{:keys [warehouse robot movements]}
        (parse-input input {:scale? (= part :p2)})]
    (sum-of-boxes (reduce (fn [state move] (step state move))
                          {:wh warehouse
                           :robot robot}
                          (map directions movements)))))

;; part 1 (79.365511 msecs)
(solve :p1 input); 1383666

;; part 2 (95.906363 msecs)
(solve :p2 input) ; 1412866
