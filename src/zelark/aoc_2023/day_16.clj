(ns zelark.aoc-2023.day-16
  (:require [zelark.aoc.core :as aoc]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 16: The Floor Will Be Lava ---
;; https://adventofcode.com/2023/day/16

(def input (aoc/get-input 2023 16))

(def f-mirror \/)
(def b-mirror \\)
(def h-splitter \-)
(def v-splitter \|)

(defn reflect [beam direction]
  (assoc beam :direction direction))

(defn encounter [{:keys [direction] :as beam} object]
  (condp = [object direction]
    [f-mirror g2/right]   (list (reflect beam g2/up))
    [f-mirror g2/left]    (list (reflect beam g2/down))
    
    [b-mirror g2/right]   (list (reflect beam g2/down))
    [b-mirror g2/left]    (list (reflect beam g2/up))
    
    [f-mirror g2/up]      (list (reflect beam g2/right))
    [f-mirror g2/down]    (list (reflect beam g2/left))
    
    [b-mirror g2/up]      (list (reflect beam g2/left))
    [b-mirror g2/down]    (list (reflect beam g2/right))

    [v-splitter g2/right] (map #(reflect beam %) [g2/up g2/down])
    [v-splitter g2/left]  (map #(reflect beam %) [g2/up g2/down])
    
    [h-splitter g2/up]    (map #(reflect beam %) [g2/left g2/right])
    [h-splitter g2/down]  (map #(reflect beam %) [g2/left g2/right])

    (list beam)))

(defn move [{:keys [direction] :as beam}]
  (update beam :loc g2/plus direction))

(defn step [layout state]
  (let [{:keys [heat-map beams]} state
        beams' (reduce (fn [acc beam]
                         (let [beam' (move beam)
                               loc   (beam' :loc)
                               x     (layout loc)]
                           (cond
                             (nil? x) acc             ; out of boundary
                             (and (g2/empty-space? x) ; a loop
                                  (when-let [energized? (heat-map loc)]
                                    (energized? (beam' :direction)))) acc
                             (g2/empty-space? x) (conj acc beam')
                             :else (into acc (encounter beam' x)))))
                       #{}
                       beams)]
    {:beams    beams'
     :heat-map (reduce #(update %1 (%2 :loc) (fnil conj #{}) (%2 :direction))
                       heat-map
                       beams')}))

(defn energized-tiles [layout beam]
  (let [init     {:beams #{beam} :heat-map {}}
        step-fn  (partial step layout)]
    (->> (iterate step-fn init)
         (drop-while #(seq (% :beams)))
         (first)
         (:heat-map))))

;; part 1 (51.177529 msecs)
(let [layout (g2/parse input)
      beam   {:direction g2/right :loc [-1 0]}]
  (count (energized-tiles layout beam))) ; 6902

;; part 2 (1372.487222 msecs)
(let [layout (g2/parse input)
      [[x1 y1] [x2 y2]] (g2/extend-boundaries (g2/boundaries layout))]
  (->> (concat (for [y* (range (inc y1) y2)] {:loc [x1 y*] :direction g2/right})
               (for [y* (range (inc y1) y2)] {:loc [x2 y*] :direction g2/left})
               (for [x* (range (inc x1) x2)] {:loc [x* y1] :direction g2/down})
               (for [x* (range (inc x1) x2)] {:loc [x* y2] :direction g2/up}))
       (pmap #(count (energized-tiles layout %)))
       (apply max))) ; 7697

;; ascii visualization (part 1)
(comment
  (let [dir->ch {g2/right \>, g2/left \<, g2/up \^, g2/down \v}
        layout  (g2/parse input)
        beam    {:direction g2/right :loc [-1 0]}]
    (->> (energized-tiles layout beam)
         (reduce-kv (fn [lt k v]
                      (let [n (count v)]
                        (if (g2/empty-space? (lt k))
                          (if (aoc/one? n) (assoc lt k (dir->ch (first v))) (assoc lt k n))
                          lt)))
                    layout)
         (aoc/print-points-2))))
