(ns zelark.aoc-2023.day-20
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 20: Pulse Propagation ---
;; https://adventofcode.com/2023/day/20

(def input (aoc/get-input 2023 20))

(defn parse-module [s]
  (case (first s)
    \% [(subs s 1) :flip-flop]
    \& [(subs s 1) :conj]
    \b [s :broadcast]))

(defn update-input [cfg modules input]
  (reduce #(update-in %1 [%2 :input] (fnil conj []) input) cfg modules))

(defn parse [input]
  (->> (str/split-lines input)
       (reduce (fn [cfg line]
                 (let [[from to] (str/split line #" -> ")
                       [name type] (parse-module from)
                       modules (str/split to #", ")]
                   (-> cfg
                       (assoc-in [name :type] type)
                       (assoc-in [name :output] modules)
                       (update-input modules name))))
               {})))

(def low 0)
(def high 1)
(def high-pulse? aoc/one?)
(def low-pulse? zero?)

(defn push-button [cfg state]
  (let [->messages (fn [from to pulse] (map #(-> [from % pulse]) to))
        {:keys [pulses times]} (meta state)]
    (loop [queue  (aoc/queue ["button" "broadcaster" low])
           state  state
           pulses pulses]
      (if-let [[sender receiver pulse] (peek queue)]
        (let [{:keys [type output]} (cfg receiver)
              pulses' (update pulses pulse inc)]
          (case type
            :broadcast
            (let [ms (->messages receiver output pulse)]
              (recur (into (pop queue) ms) state pulses'))

            :flip-flop
            (if (low-pulse? pulse)
              (let [state' (update state receiver not)
                    pulse' (if (state' receiver) high low)
                    ms     (->messages receiver output pulse')]
                (recur (into (pop queue) ms) state' pulses'))
              (recur (pop queue) state pulses'))

            :conj
            (let [state' (assoc-in state [receiver sender] pulse)
                  pulse' (if (every? high-pulse? (vals (state' receiver)))
                           low
                           high)
                  ms     (->messages receiver output pulse')]
              (recur (into (pop queue) ms) state' pulses'))

            ;; untyped (for testing purposes, actually not ^_^)
            (recur (pop queue) state pulses')))
        (with-meta state {:pulses pulses :times (inc times)})))))

(defn init-state [config]
  (reduce-kv (fn [state module-name module]
               (case (module :type)
                 :flip-flop (assoc state module-name false)
                 :conj      (assoc state module-name (zipmap (module :input) (repeat low)))
                 state))
             (with-meta {} {:pulses {low 0 high 0} :times 0})
             config))

;; part 1 (86.967126 msecs)
(let [config (parse input)
      state  (->> (iterate #(push-button config %) (init-state config))
                  (drop 1000)
                  (first))
      pulses (-> state meta :pulses)]
  (aoc/mul val pulses)) ; 886347020

;; part 2 (289.383215 msecs)
(defn find-loop [pushes ff-module]
  (->> (map #(-> {:times (:times (meta %)) :val (% ff-module)}) pushes)
       (partition 2 1)
       (some (fn [[a b]] (when (= (:val a) (:val b)) (:times b))))))

(let [config (parse input)
      state  (init-state config)
      pushes (iterate #(push-button config %) state)]
  (->> (get-in config ["broadcaster" :output]) ; flip-flops connected to broadcaster
       (map #(find-loop pushes %))
       (apply aoc/lcm))) ; 233283622908263
