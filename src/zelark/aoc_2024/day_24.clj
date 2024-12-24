(ns zelark.aoc-2024.day-24
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 24: Crossed Wires ---
;; https://adventofcode.com/2024/day/24

(def input (aoc/get-input 2024 24))

(defn parse-wire [s]
  (let [[n v] (str/split s #": ")]
    [n (if (= v "1") true false)]))

(defn parse-gate [s]
  (let [[in1 gate in2 _ _ out] (str/split s #"\s|->")]
    {out {:inputs [in1 in2] :gate gate}}))

(defn parse-input [input]
  (let [[wires gates] (aoc/split-on-blankline input)]
    {:wires (->> (str/split-lines wires)
                 (map parse-wire)
                 (reduce (fn [m [k v]] (assoc m k v)) {}))
     :gates (->> (str/split-lines gates)
                 (mapv parse-gate)
                 (reduce merge))}))

;; https://blog.exupero.org/topological-sort-in-clojure/
(defn topo-sort [graph]
  (when (seq graph)
    (when-let [ks (keep (fn [[k v]] (when (empty? v) k)) graph)]
      (concat ks
              (topo-sort
               (into {}
                     (map (fn [[k v]] [k (apply disj v ks)]))
                     (apply dissoc graph ks)))))))

(defn ->graph [gates wires]
  (reduce-kv (fn [g k v]
               (assoc g k (set (v :inputs))))
             (update-vals wires (constantly #{}))
             gates))

(defn ->dec [w values]
  (->> (map #(format "%s%02d" w %) (range 45 (dec 0) -1))
       (map values)
       (map #(if % 1 0))
       (str/join)
       (aoc/parse-bin)))

(defn xor [a b] (if (= a b) false true))

(defn result [gates wires]
  (->> (topo-sort (->graph gates wires))
       (filter gates)
       (reduce (fn [values wire]
                 (let [{:keys [inputs gate]} (gates wire)
                       [i1 i2] (map values inputs)
                       value   (case gate
                                 "AND" (and i1 i2)
                                 "OR"  (or i1 i2)
                                 "XOR" (xor i1 i2))]
                   (assoc values wire value)))
               wires)
       (->dec \z)))

;; part 1 (11.48979 msecs)
(let [{:keys [gates wires]} (parse-input input)]
  (result gates wires)) ; 66055249060558

(defn swap [gates a b]
  (assoc gates a (gates b) b (gates a)))

;; part 2 ()
(let [{:keys [gates wires]} (parse-input input)
      expected (+ (->dec \x wires) (->dec \y wires))
      gates    (-> gates
                   (swap "hmk" "z16")
                   (swap "fhp" "z20")
                   (swap "rvf" "tpc") ; z27
                   (swap "fcd" "z33"))
      actual   (result gates wires)]
  (= actual expected))

(defn g->gviz [gates wires]
  (doseq [[wire value] wires]
    (println (format "\"%s\" [label=\"%s\\n\\n%s\"]" wire wire (if value 1 0))))
  (doseq [[output {:keys [inputs gate]}] gates
          :let [[i1 i2] inputs]]
    (println (format "\"%s\" [label=\"%s\\n\\n%s\"]" output output gate))
    (println (format "\"%s\" -> \"%s\" [dir=arrow]" i1 output))
    (println (format "\"%s\" -> \"%s\" [dir=arrow]" i2 output))))

(let [{:keys [gates wires]} (parse-input input)]
  (g->gviz gates wires))

;; https://edotor.net/

(->> (sort ["fhp" "z20"
            "rvf" "tpc"
            "hmk" "z16"
            "fcd" "z33"])
     (str/join ",")) ; fcd,fhp,hmk,rvf,tpc,z16,z20,z33


;;           444444333333333322222222221111111111
;;           5432109876543210987654321098765432109876543210
;; actual:   1111000001001110101111101001111110001011001110
;; expected: 1111000001010110110111100110001110001011001110


;; half adder
;; X1 XOR Y1 -> M1
;; X1 AND Y1 -> N1
;; C0 XOR M1 -> Z1
;; C0 AND M1 -> R1
;; R1 OR N1  -> C1
