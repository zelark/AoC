(ns zelark.aoc-2024.day-21
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [zelark.aoc.grid-2d :as g2]))

;; --- Day 21: Keypad Conundrum ---
;; https://adventofcode.com/2024/day/21

(def input (aoc/get-input 2024 21))

(def numeric-keypad "789\n456\n123\n#0A")
(def directional-keypad "#^A\n<v>")

(defn parse-input [input]
  (let [codes (str/split-lines input)
        nums  (aoc/parse-longs input)]
    (mapv vector codes nums)))

(def dir->button {g2/up \^ g2/down \v g2/right \> g2/left \<})

(defn ->button [a b]
  (dir->button (g2/minus b a)))

(defn neighbors
  "Returns neighbors. Order matters!"
  [grid loc]
  (->> (map #(g2/plus loc %) [g2/left g2/up g2/down g2/right])
       (filter grid)))

(defn find-paths [neighbors start goal]
  (let [dfs (fn dfs [path current seen]
              (if (= current goal)
                [path]
                (->> (neighbors current)
                     (remove seen)
                     (mapcat #(dfs (conj path (->button current %)) % (conj seen %))))))
        pref (fn [p] (aoc/sum (map #(if (not= %1 %2) 1 0) p (rest p))))]
    (->> (dfs [] start #{start})
         (sort-by pref)
         (first))))

(defn build-keypad-map [keypad]
  (let [grid      (g2/parse keypad (g2/any-but \#))
        neighbors (partial neighbors grid)]
    (->> (for [[from a] grid
               [to b]   grid]
           [[a b] (str/join (conj (find-paths neighbors from to) \A))])
         (reduce (fn [m [k v]] (assoc m k v)) {}))))

(def numeric-keypad-map (build-keypad-map numeric-keypad))
(def directional-keypad-map (build-keypad-map directional-keypad))

(def from-A #(str "A" %))

(defn numeric-seq [code]
  (-> (map #(numeric-keypad-map %) (partition 2 1 (from-A code)))
      (str/join)))

(def length
  (memoize
   (fn [ds limit]
     (if (zero? limit)
       (count ds)
       (->> (partition 2 1 (from-A ds))
            (map #(directional-keypad-map %))
            (map #(length % (dec limit)))
            (aoc/sum))))))

(defn solve [input limit]
  (->> (parse-input input)
       (aoc/sum (fn [[code num]]
                  (* (length (numeric-seq code) limit) num)))))

;; part 1 (1.976242 msecs -> 0.481756 msecs)
(solve input 2) ; 278748

;; part 2 (4.664089 msecs -> 0.506844 msecs)
(solve input 25) ; 337744744231414
