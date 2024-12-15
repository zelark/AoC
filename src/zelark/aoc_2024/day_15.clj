(ns zelark.aoc-2024.day-15
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
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

(defn box? [tile]
  (or (= tile \O)
      (= tile \[)
      (= tile \])))

(defn wall? [tile]
  (= tile \#))

(defn sum-of-boxes [{:keys [wh]}]
  (aoc/sum (fn [[[x y] tile]]
             (if (#{\O \[} tile) (+ (* 100 y) x) 0))
           wh))

;; part 1 (31.262586 msecs)
(defn move-boxes [wh loc prev dir]
  (let [new-loc (g2/plus loc dir)
        tile    (wh new-loc)]
    (cond
      (wall? tile) nil

      (g2/empty-space? tile)
      (assoc wh
             loc     prev
             new-loc (wh loc))

      (box? tile)
      (recur (assoc wh loc prev) new-loc (wh loc) dir))))

(defn step [{:keys [wh robot] :as state} move]
  (let [dir     (directions move)
        new-loc (g2/plus robot dir)
        tile    (wh new-loc)]
    (cond
      (wall? tile)
      state
      
      (g2/empty-space? tile)
      (assoc state :robot new-loc)
      
      (box? tile)
      (if-let [wh' (move-boxes wh new-loc g2/empty-space dir)]
        {:wh wh' :robot new-loc}
        state))))

(let [{:keys [warehouse robot movements]} (parse-input input)]
  (sum-of-boxes (reduce (fn [state move] (step state move))
                        {:wh warehouse
                         :robot robot}
                        movements))) ; 1383666

;; part 2 (62.738664 msecs)
(defn place-tiles [wh locs tiles]
  (->> (map vector locs tiles)
       (reduce (fn [w [loc tile]] (assoc w loc tile)) wh)))

(defn adjust-locs [wh locs]
  (let [locs (loop [locs locs
                    ret  []]
               (if (seq locs)
                 (let [loc (first locs)]
                   (if (and (g2/empty-space? (wh loc))
                            (g2/empty-space? (wh (second locs))))
                     (recur (nnext locs) ret)
                     (recur (next locs) (conj ret loc))))
                 ret))
        [a b] ((juxt first last) locs)
        locs (cond->> locs
               (= (wh a) \]) (cons (g2/plus a g2/left))
               (= (wh a) \.) (rest))
        locs (cond-> locs
               (= (wh b) \[) (concat [(g2/plus b g2/right)])
               (= (wh b) \.) (butlast))]
    locs))

(defn move-boxes-v [wh locs prevs dir]
  (let [adj-locs (adjust-locs wh locs)
        new-locs (map #(g2/plus % dir) adj-locs)
        tiles    (map wh new-locs)]
    (cond
      (some wall? tiles) nil
      
      (every? g2/empty-space? tiles) 
      (-> wh
          (place-tiles adj-locs (repeat (count adj-locs) \.))
          (place-tiles locs prevs)
          (place-tiles new-locs (map wh adj-locs)))
      
      (some box? tiles)
      (recur (-> wh
                 (place-tiles adj-locs (repeat (count adj-locs) \.))
                 (place-tiles locs prevs))
             new-locs
             (map wh adj-locs)
             dir))))

(defn step-2 [{:keys [wh robot] :as state} move]
  (let [dir     (directions move)
        new-loc (g2/plus robot dir)
        tile    (wh new-loc)]
    (cond
      (wall? tile) state
      
      (g2/empty-space? tile)
      (assoc state :robot new-loc)

      (and (box? tile) (#{g2/left g2/right} dir))
      (if-let [wh' (move-boxes wh new-loc g2/empty-space dir)]
        {:wh wh' :robot new-loc}
        state)

      (and (box? tile) (#{g2/up g2/down} dir))
      (if-let [wh' (move-boxes-v wh (adjust-locs wh [new-loc]) [g2/empty-space g2/empty-space] dir)]
        {:wh wh' :robot new-loc}
        state))))

(defn print-map* [{:keys [wh robot]}]
  (aoc/print-points-2 (assoc wh robot \@)))

(let [{:keys [warehouse robot movements]}
      (parse-input input {:scale? true})]
  (sum-of-boxes (reduce (fn [state move] (step-2 state move))
                        {:wh warehouse
                         :robot robot}
                        movements))) ; 1412866