(ns zelark.aoc.graph)

(defn shortest-paths
  "Floyd–Warshall algorithm"
  ([g] (shortest-paths g (constantly 1)))
  ([g w]
   (let [init (into (reduce (fn [m v] (assoc m [v v] 0)) {} (keys g))
                    (for [[v1 vs] g, v2 vs] [[v1 v2] (w [v1 v2])]))]
     (reduce (fn [dist [k i j]]
               (assoc dist [i j]
                      (min (get dist [i j] ##Inf)
                           (+ (get dist [i k] ##Inf)
                              (get dist [k j] ##Inf)))))
             init
             (for [k (keys g), i (keys g), j (keys g)] [k i j])))))
