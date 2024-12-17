(ns zelark.aoc-2024.day-17
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.math :as math]
            [zelark.aoc.grid-2d :as g2]
            [zelark.aoc.graph :as g]))

;; --- Day 17: Chronospatial Computer ---
;; https://adventofcode.com/2024/day/17

(def input (aoc/get-input 2024 17))

(def example "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0")

(defn parse-input [input]
  (let [[regs prog] (aoc/split-on-blankline input)]
    {:registers (zipmap [:a :b :c] (aoc/parse-longs regs))
     :program   (aoc/parse-longs prog)}))

(defn combo-operand [state operand]
  (case operand
    (0 1 2 3) operand
    4 (state :a)
    5 (state :b)
    6 (state :c)
    7 (throw (ex-info "7 reserved and will not appear in valid programs."
                      {:state state}))))

(defn run-code [regs code]
  (loop [state (assoc regs :out [])
         ip    0]
    (let [opcode (nth code ip -1)]
      #_(prn :opcode opcode :ip ip :state state)
      (if (neg? opcode)
        state
        (let [operand (combo-operand state (nth code (inc ip)))]
          (case opcode
            ;; adv
            0  (let [ret (long (/ (:a state)
                                  (bit-shift-left 1 operand)))]
                 (recur (assoc state :a ret) (+ ip 2)))
            ;; bxl
            1  (let [ret (bit-xor (:b state) operand)]
                 (recur (assoc state :b ret) (+ ip 2)))
            ;; bst
            2  (let [ret (bit-and operand 2r111)]
                 (recur (assoc state :b ret) (+ ip 2)))
            ;; jnz
            3  (if (zero? (:a state))
                 (recur state (+ ip 2))
                 (recur state operand))
            ;; bxc
            4  (let [ret (bit-xor (:b state) (:c state))]
                 (recur (assoc state :b ret) (+ ip 2)))
            ;; out
            5  (let [ret (bit-and operand 2r111)]
                 (recur (update state :out conj ret) (+ ip 2)))
            ;; bdv
            6  (let [ret (long (/ (:a state)
                                  (bit-shift-left 1 operand)))]
                 (recur (assoc state :b ret) (+ ip 2)))
            ;; cdv
            7  (let [ret (long (/ (:a state)
                                  (bit-shift-left 1 operand)))]
                 (recur (assoc state :c ret) (+ ip 2)))))))))

;; If register C contains 9, the program 2,6 would set register B to 1.
;; If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
;; If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
;; If register B contains 29, the program 1,7 would set register B to 26.
;; If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.

(run-code {:a 0 :b 0 :c 9} [2 6]) ; b 1
(run-code {:a 10 :b 0 :c 0} [5,0,5,1,5,4]) ; out [0 1 2]
(run-code {:a 2024 :b 0 :c 0} [0,1,5,4,3,0]) ; out [4 2 5 6 7 7 7 7 3 1 0] and a 0
(run-code {:a 0 :b 29 :c 0} [1 7]) ; b 26 ?!
(run-code {:a 0 :b 2024 :c 43690} [4 0]) ; b 44354

;; part 1 ()
(parse-input input)

(let [{:keys [registers program]} (parse-input input)]
  (->> (run-code registers program)
       :out
       (str/join ",")))

; Wrong answers
; 6,6,2,1,4,3,3,1,0
; 6,5,0,4,1,0,0
; 4,4,4,3,0,6,6,2,1


;; part 2 ()


