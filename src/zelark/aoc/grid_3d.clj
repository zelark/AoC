(ns zelark.aoc.grid-3d)

(defn neighbors [[x y z]]
  (for [[dx dy dz] [[-1  0  0] [1 0 0]
                    [ 0 -1  0] [0 1 0]
                    [ 0  0 -1] [0 0 1]]]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn all-neighbors [[x y z]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        dz [-1 0 1]
        :when (not= 0 dx dy dz)]
    [(+ x dx) (+ y dy) (+ z dz)]))

(defn boundaries [points]
  (let [min-x (apply min (map #(nth % 0) points))
        max-x (apply max (map #(nth % 0) points))
        min-y (apply min (map #(nth % 1) points))
        max-y (apply max (map #(nth % 1) points))
        min-z (apply min (map #(nth % 2) points))
        max-z (apply max (map #(nth % 2) points))]
    [[min-x min-y min-z]
     [max-x max-y max-z]]))

(defn extend-boundaries [[lower upper]]
  [(mapv dec lower)
   (mapv inc upper)])

(defn inside? [boundaries [x y z]]
  (let [[[xl yl zl] [xr yr zr]] boundaries]
    (and (<= xl x xr)
         (<= yl y yr)
         (<= zl z zr))))
