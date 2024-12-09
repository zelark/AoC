(ns zelark.aoc-2024.day-09
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 9: Disk Fragmenter ---
;; https://adventofcode.com/2024/day/9

(def input (aoc/get-input 2024 9))

(defn span [n] (vec (repeat n \.)))

(defn parse-input [input]
  (let [ids (->> (cycle [:space])
                 (interleave (range)))]
    (->> (map (fn [id n]
                (if (= id :space)
                  (span n)
                  (vec (repeat n id))))
              ids (map aoc/ch->digit input))
         (remove empty?)
         (vec))))

(defn calc-checksum [disk]
  (->> (keep-indexed (fn [pos id] (when (number? id)
                                    (* pos id))) disk)
       (aoc/sum)))

(defn place-file [blocks file]
  (loop [ret []
         blocks blocks
         file file]
    (if (seq file)
      (if (= (first blocks) \.)
        (recur (conj ret (first file))
               (next blocks)
               (next file))
        (recur (conj ret (first blocks))
               (next blocks)
               file))
      (into ret blocks))))

(defn find-next-idx-with-free-space [disk i]
  (reduce (fn [acc n]
            (if (some #{\.} (nth disk n))
              (reduced n)
              acc))
          i
          (range i (count disk))))

;; part 1 (74.24439 msecs)
(defn split-file [file i]
  [(subvec file 0 i) (subvec file i (count file))])

(let [disk (parse-input input)]
  (->> (loop [ret disk
              i (find-next-idx-with-free-space ret 0)
              j (dec (count disk))]
         (let [current (nth ret i)
               target  (nth ret j)
               file?   (every? number? target)]
           (cond
             (<= j i) ret

             file?
             (let [free-space (aoc/cnt current \.)
                   size       (count target)]
               (if (<= size free-space)
                 (let [ret' (assoc ret
                                   i (place-file current target)
                                   j (span size))]
                   (recur ret'
                          (find-next-idx-with-free-space ret' i)
                          (dec j)))
                 (let [[p1 p2] (split-file target free-space)
                       ret' (assoc ret
                                   i (place-file current p1)
                                   j p2)]
                   (recur ret'
                          (find-next-idx-with-free-space ret' i)
                          j))))

             :else
             (recur ret i (dec j)))))
       (mapcat identity)
       (calc-checksum))) ; 6279058075753

;; part 2 (3817.592484 msecs)
(defn move-file [disk file size i j]
  (loop [i i]
    (let [current (nth disk i)]
      (cond 
        (<= j i) nil
        (<= size (aoc/cnt current \.)) [i (place-file current file)]
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
             (if-let [[k chunk] (move-file ret target size i j)]
               (let [ret' (assoc ret
                                 k chunk
                                 j (vec (repeat size \.)))]
                 (recur ret'
                        (find-next-idx-with-free-space ret' i)
                        (dec j)
                        failed?))
               (recur ret i (dec j) (conj failed? size)))

             :else
             (recur ret i (dec j) failed?))))
       (mapcat identity)
       (calc-checksum))) ; 6301361958738
