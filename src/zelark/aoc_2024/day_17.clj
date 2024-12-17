(ns zelark.aoc-2024.day-17
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 17: Chronospatial Computer ---
;; https://adventofcode.com/2024/day/17

(def input (aoc/get-input 2024 17))

(defn parse-input [input]
  (let [[regs prog] (aoc/split-on-blankline input)]
    {:registers (zipmap [:a :b :c] (aoc/parse-longs regs))
     :program   (aoc/parse-longs prog)}))

(defn combo-operand [state operand]
  (case operand
    (0 1 2 3) operand
    4 (state :a)
    5 (state :b)
    6 (state :c)))

(defn run-code [regs code]
  (loop [state (assoc regs :out [])
         ip    0]
    (let [opcode (nth code ip -1)]
      (if (neg? opcode)
        (:out state)
        (let [oprand (nth code (inc ip))
              combo  (combo-operand state oprand)]
          (case opcode
            ;; adv
            0  (recur (assoc state :a (bit-shift-right (:a state) combo))
                      (+ ip 2))
            ;; bxl
            1  (recur (assoc state :b (bit-xor (:b state) oprand))
                      (+ ip 2))
            ;; bst
            2  (recur (assoc state :b (bit-and combo 2r111))
                      (+ ip 2))
            ;; jnz
            3  (if (zero? (:a state))
                 (recur state (+ ip 2))
                 (recur state oprand))
            ;; bxc
            4  (recur (assoc state :b (bit-xor (:b state) (:c state)))
                      (+ ip 2))
            ;; out
            5  (recur (update state :out conj (bit-and combo 2r111))
                      (+ ip 2))
            ;; bdv
            6  (recur (assoc state :b (bit-shift-right (:a state) combo))
                      (+ ip 2))
            ;; cdv
            7  (recur (assoc state :c (bit-shift-right (:a state) combo))
                      (+ ip 2))))))))

;; part 1 (1.541395 msecs)
(let [{:keys [registers program]} (parse-input input)]
  (->> (run-code registers program)
       (str/join ","))) ; 5,0,3,5,7,6,1,5,4

;; part 2 (13.703978 msecs)
(let [{:keys [registers program]} (parse-input input)]
  (loop [stack [[0 (dec (count program))]]]
    (when-let [[a n] (peek stack)]
      (if (neg? n)
        a
        (->> (for [a* (range (* a 8) (+ (* a 8) 8))
                   :let [out (run-code (assoc registers :a a*) program)]
                   :when (= out (drop n program))]
               [a* (dec n)])
             (reverse) ; Reversing needed because of the stack.
             (reduce conj (pop stack))
             (recur))))))

;; An analyzed code to figure out what's going on here.
; 2,4 - bst: (mod a 8)  -> b
; 1,1 - bxl: (xor b 1)  -> b
; 7,5 - cdv: (/ a 2^b)  -> c
; 1,5 - bxl: (xor b 5)  -> b
; 0,3 - adv: (/ a 2^3)  -> a
; 4,4 - bxc: (xor b c)  -> b
; 5,5 - out: (mod b 8)  -> out
; 3,0 - jmp: to 0 if a not zero (loop)
