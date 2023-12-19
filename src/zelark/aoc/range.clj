(ns zelark.aoc.range
  (:refer-clojure :exclude [range contains?]))

(defrecord Range [start end len]
  Object
  (toString [_] (str [start end])))

(defmethod print-method Range
  [object writer]
  (.append writer (format "[%s %s]" (.start object) (.end object))))

(defn rangee
  "Creates a range from start (inclusive) to end."
  ([range]
   (cond
     (vector? range)
     (rangee (first range) (second range))
     
     (instance? Range range)
     range
     
     :else
     (throw (IllegalArgumentException. (format "Don't know how to crate a %s from %s" Range range)))))
  ([start end]
   (assert (< start end) "start must be less then end.")
   (map->Range {:start start
                :end   end
                :len   (- end start)})))

(defn rangel [start len]
  (rangee start (+ start len)))

(defn contains? [range x]
  (and (<= (:start range) x)
       (< x (:end range))))

(defn split [range n]
  (when (contains? range n)
    (let [{:keys [start end]} range]
      [(rangee start n) (rangee n end)])))

(defn add-offset [range offset]
  (rangel (+ (:start range) offset)
          (:len range)))

(defn- exclude-one
  ([r1 r2]
   (let [left  (when (< (:start r1) (:start r2))
                 (rangee (:start r1) (min (:end r1) (:start r2))))
         right (when (< (:end r2) (:end r1))
                 (rangee (max (:start r1) (:end r2)) (:end r1)))]
     [left right])))

(defn exclude [range & ranges]
  (->> (sort-by :start ranges)
       (reduce (fn [[acc r1] r2]
                 (let [[prefix suffix] (exclude-one r1 r2)]
                   [(cond-> acc prefix (conj prefix)) suffix]))
               [[] range])
       (apply conj)))

(defn length [range]
  (:len range))

(defn intersect-with [r1 r2]
  (let [start (max (:start r1) (:start r2))
        end   (min (:end r2) (:end r2))]
    (when (< start end)
      (rangee start end))))

(comment
  (rangel 1 10) ; => [1 10]
  (rangee 10 1) ; => AssertionError
  
  (str (rangel 1 10))
  
  (add-offset (rangel 1 4000) 10)
  
  (contains? (rangel 1 10) 10) ; => true
  (contains? (rangee 1 10) 10) ; => false
  
  (split (rangel 1 4000) 1000)
  (length (first (split (rangel 1 4000) 4000)))
  
  (length (rangel 1 4000))
  
  (intersect-with (rangel 1 10)
                  (rangel 5 12))

  (exclude (rangel 1 9) (rangel 2 5))
  (exclude (rangel 1 9) (rangel -3 3)) 
  (exclude (rangel 1 9) (rangel 2 5) (rangel -2 5))
  
  (rangee 1)
  )
