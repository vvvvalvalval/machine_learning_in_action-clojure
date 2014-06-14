(ns utils.algo
  (:import [java.util TreeSet Comparator]
           )
  (:use clojure.pprint
        clojure.repl))

;; COMPARATORS

(defn- comparator-from 
  "Creates a java.util.Comparator from the arity-2 integer-valued compare fn."
  [compare-fn]
  (proxy [Comparator] []
    (compare [x y]
      (compare-fn x y))
    ))

(defn scores-comparer 
  "Creates an integer-valued arity-2 comparator function from the number-valued arity-1 score function."
  [score-fn]
  (fn compare [x y]
    (let [d (- (score-fn x) (score-fn y))]
      (cond 
        (> d 0) 1
        (< d 0) -1
        :else (if (= x y) 0 -1)
        ))))

(defn k-best 
  "Finds the k best (i.e smallest) DISTINCT items in the `items` sequence with regards to the `compare` function."
  [compare k items]
  (let [k-best (TreeSet. (comparator-from compare))]
    (doseq [item items]
      (.add k-best item)
      (when (> (.size k-best) k)
        (.pollLast k-best))
      )
    (vec k-best)))

;; MAPS
(defn transform-vals 
  "Creates a new map from the map `m` by applying `f` to its values."
  [f m]
  (reduce 
    (fn [acc [k v]] (assoc acc k (f v)))
    {} m))

(defn transform-keys "Creates a new map from the map `m` by applying `f` to its keys. 
(the result is unpredicted if f is not injective)." 
  [f m]
  (reduce 
    (fn [acc [k v]] (assoc acc (f k) v))
    {} m))

;; MISC
(defn search-max
  "Returns the first `[item, (score item)]` tuple computed from the items of `coll` with highest score.

Examples : 
  (search-max {:a 1 :b 3 :c 2 :d -1}
            [:a :b :c]) => [:b 3]
  (search-max #(* % %)
            [1 2 3 2 -5 6 -8 -2 7]) => [-8 64]
"
  [score coll]
  (loop [best (first coll), max (score best), rem (next coll)]
    (if rem
      (let [x (first rem), sc (score x)]
        (if (> sc max)
          (recur x sc (next rem))
          (recur best max (next rem))))
      [best max])))
(comment 
  (search-max {:a 1 :b 3 :c 2 :d -1}
            [:a :b :c]) => [:b 3]
  (search-max #(* % %)
            [1 2 3 2 -5 6 -8 -2 7]) => [-8 64]
  )
(def argmax (comp first search-max))

