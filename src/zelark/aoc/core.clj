(ns zelark.aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [clojure.edn :as edn])
  (:import [clojure.lang PersistentQueue]))

(defn get-input
  ([year day & [option]]
   (let [path-to-file (io/resource (format "%d/input_%02d.txt" year day))
         load-input #(-> % slurp str/trim-newline)]
     (if (or (not (.exists (io/file path-to-file)))
             (= option :force-load))
       (do (println "Downloading" (str path-to-file))
           (shell/sh "./bin/fetch-input" (str year) (str day))
           (load-input path-to-file))
       (load-input path-to-file)))))

(defn queue [& args]
  (into PersistentQueue/EMPTY args))

;; Parsing
(defn parse-longs [s]
  (->> (re-seq #"-?\d+" s)
       (mapv parse-long)))

(defn parse-bin [s]
  (Long/parseLong s 2))

(defn split-on-blankline [input]
  (str/split input #"\R\R"))

(defn parse-asm-code [input]
  (->> (str/split-lines input)
       (mapv #(str "[" % "]"))
       (mapv edn/read-string)
       (mapv (fn [x] (mapv #(cond-> % (symbol? %) keyword) x)))))

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

(defn mul [xs]
  (reduce * 1 xs))

(defn fix-point [f x]
  (let [x' (f x)]
    (if (= x x') x (recur f x'))))

(defn transpose [v]
  (apply mapv vector v))

;; Grids
(defn grid-get [grid [x y]]
  (get-in grid [y x]))

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

;; path finding
(defn- generate-route [node came-from]
  (loop [route ()
         node node]
    (if (came-from node)
      (recur (cons node route) (came-from node))
      route)))

(defn bfs [graph start goal?]
  (loop [seen  {start nil}
         queue (queue start)
         n     0]
    (when-let [current (peek queue)]
      (if (goal? current)
        (generate-route current seen)
        (let [[seen queue] (reduce (fn [[seen queue :as acc] node]
                                     (if (contains? seen node)
                                       acc
                                       [(assoc seen node current) (conj queue node)]))
                                   [seen (pop queue)]
                                   (graph current))]
          (recur seen queue (inc n)))))))
