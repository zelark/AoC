(ns zelark.aoc.circular-list
  (:refer-clojure :exclude [remove]))

(defprotocol ICircularNode
  (value    [this])
  (get-next [this])
  (get-prev [this])
  (set-prev [this node])
  (set-next [this node]))

(deftype CircularNode [value ^:volatile-mutable next ^:volatile-mutable prev]
  ICircularNode
  (value [_] value)
  (get-next [_] next)
  (set-next [this node]
    (set! next node)
    this)
  (get-prev [_] prev)
  (set-prev [this node]
    (set! prev node)
    this))

(defn insert-after
  "Inserts a `node` after `base`. Returns `base`. 
  
  Example: base <-> base-next  =>  base <-> node <-> base-next."
  [base node]
  (let [base-next (get-next base)]
    (set-next node base-next)
    (set-prev node base)
    (set-prev base-next node)
    (set-next base node)
    base))

(defn remove
  "Removes a given `node` from the list. Returns the `node`.
   
  Example: prev <-> node <-> next  =>  prev <-> next."
  [node]
  (let [prev (get-prev node)
        next (get-next node)]
    (set-next prev next)
    (set-prev next prev)
    node))

(defn move-after
  "Moves a given `node` after `base`. Returns the `node`.
   
  Example: prev <-> [node] <-> next ...  base <-> base-next
           =>  prev <-> next  ...  base <-> node <-> base-next."
  [base node]
  (remove node)
  (insert-after base node)
  node)

(defn circular-node [value]
  (let [node (CircularNode. value nil nil)]
    (set-next node node)
    (set-prev node node)))

(defn circular-list [coll]
  (first (reduce (fn [[nodes prev] x]
                   (let [node (circular-node x)
                         _    (when prev (insert-after prev node))]
                     [(conj nodes node) node]))
                 [[] nil]
                 coll)))
