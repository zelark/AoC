(ns zelark.aoc.graph
  (:import [clojure.lang PersistentQueue]))

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

(defn- bfs-seq
  ([neighbors start]
   (bfs-seq neighbors (conj PersistentQueue/EMPTY [start]) #{start}))
  ([neighbors queue seen]
   (when-let [current (peek queue)]
     (cons current
           (lazy-seq
            (let [[queue seen]
                  (->> (neighbors (peek current))
                       (reduce (fn [[q s] node]
                                 (if (seen node)
                                   [q s]
                                   [(conj q (conj current node)) (conj s node)]))
                               [(pop queue) seen]))]
              (bfs-seq neighbors queue seen)))))))

(defn bfs
  ([g start] (bfs-seq g start))
  ([g start goal]
   (let [goal? (if (or (fn? goal) (set? goal)) goal #{goal})]
     (first (filter #(goal? (peek %)) (bfs-seq g start))))))

(defn connected-group [neighbors v]
  (reduce #(conj % (peek %2)) #{} (bfs neighbors v)))

(defn connected-groups
  ([g] (connected-groups g nil))
  ([g neighbors]
   (let [neighbors (or neighbors g)]
     (reduce (fn [groups v]
               (if (some #(% v) groups)
                 groups
                 (conj groups (connected-group neighbors v))))
             []
             (keys g)))))
