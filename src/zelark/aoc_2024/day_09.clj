(ns zelark.aoc-2024.day-09
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 9: Disk Fragmenter ---
;; https://adventofcode.com/2024/day/9

(def input (aoc/get-input 2024 9))

(defn free-space [n] (vec (repeat n :free)))

(defn parse-input [input]
  (let [ids (->> (cycle [:free])
                 (interleave (range)))]
    (->> (map (fn [id n]
                (if (= id :free)
                  (free-space n)
                  (vec (repeat n id))))
              ids (map aoc/ch->digit input))
         (remove empty?)
         (vec))))

(defn calc-checksum [disk]
  (->> (keep-indexed (fn [pos id] (when (number? id) (* pos id))) disk)
       (aoc/sum)))

(defn place-file [blocks file]
  (loop [ret []
         blocks blocks
         file file]
    (if (seq file)
      (if (= (first blocks) :free)
        (recur (conj ret (first file))
               (next blocks)
               (next file))
        (recur (conj ret (first blocks))
               (next blocks)
               file))
      (into ret blocks))))

(defn find-next-chunk-idx-with-free-space [disk i]
  (reduce (fn [acc n]
            (if (some #{:free} (nth disk n))
              (reduced n)
              acc))
          i
          (range i (count disk))))

;; part 1 (74.24439 msecs)
(defn split-file [file i]
  [(subvec file 0 i) (subvec file i (count file))])

(let [disk (parse-input input)]
  (->> (loop [ret disk
              i (find-next-chunk-idx-with-free-space ret 0)
              j (dec (count disk))]
         (let [current (nth ret i)
               target  (nth ret j)
               file?   (every? number? target)]
           (cond
             (<= j i) ret

             file?
             (let [free-space-size (aoc/cnt current :free)
                   file-size       (count target)]
               (if (<= file-size free-space-size)
                 (let [ret' (assoc ret
                                   i (place-file current target)
                                   j (free-space file-size))]
                   (recur ret'
                          (find-next-chunk-idx-with-free-space ret' i)
                          (dec j)))
                 (let [[p1 p2] (split-file target free-space-size)
                       ret' (assoc ret
                                   i (place-file current p1)
                                   j p2)]
                   (recur ret'
                          (find-next-chunk-idx-with-free-space ret' i)
                          j))))

             :else
             (recur ret i (dec j)))))
       (mapcat identity)
       (calc-checksum))) ; 6279058075753

;; part 2 (3817.592484 msecs)
(defn find-place [disk file-size i j]
  (loop [i i]
    (let [chunk (nth disk i)]
      (cond 
        (<= j i) nil
        (<= file-size (aoc/cnt chunk :free)) [i chunk]
        :else (recur (inc i))))))

(let [disk (parse-input input)]
  (->> (loop [ret disk
              i 0
              j (dec (count disk))
              failed? #{}]
         (let [target (nth ret j)
               size   (count target)
               file?  (every? number? target)]
           (cond
             (<= j i) ret

             (and file?
                  (not (failed? size)))
             (if-let [[k chunk] (find-place ret size i j)]
               (let [ret' (assoc ret
                                 k (place-file chunk target)
                                 j (free-space size))]
                 (recur ret'
                        (find-next-chunk-idx-with-free-space ret' i)
                        (dec j)
                        failed?))
               (recur ret i (dec j) (conj failed? size)))

             :else
             (recur ret i (dec j) failed?))))
       (mapcat identity)
       (calc-checksum))) ; 6301361958738
