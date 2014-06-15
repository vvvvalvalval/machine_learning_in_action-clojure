(ns utils.algo
  (:import [java.util TreeMap Comparator]
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
        :else 0
        ))))

(def k-lowest 
  "Finds the k best (i.e smallest) DISTINCT items in the `items` sequence with regards to the `score` function."
  (let [sign (fn [d] 
               (cond (> d 0) 1, (< d 0) -1, :else 0))
        ^Comparator pairs-comparator (proxy [Comparator] []
                                       (compare [p1 p2]
                                         (let [d1 (- (p1 0) (p2 0))]
                                           (if (= 0 d1)
                                             (- (p1 1) (p2 1))
                                             (sign d1))))
                                       )]
    (fn [score k items]
     (let [k-best (TreeMap. pairs-comparator)]
       (loop [rem-items items
              idx 0]
         (if rem-items
           (let [item (first rem-items)]
             (.put k-best [(score item) idx] item)
             (when (> (.size k-best) k)
               (.pollLastEntry k-best))
             (recur (next rem-items)
                    (inc idx)))
           (->> k-best .values seq)))
       ))))
(comment
  (let 
    [size 10000
     pol #(* (- % 3) (- % 2.3) (+ % 5.3))
     rdm #(->> (java.lang.Math/random) (* 2) (- 1))
     items (->> (repeatedly size rdm) doall vec)]
    (time 
      (k-lowest pol 3 items))
    ) => (0.943914174845194 0.9038520034140807 0.8271175189363764)
  )

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

(defn zip [& colls]
  (apply mapv vector colls))
(comment
  (zip [1 2 3] [:a :b :c] ["You" "Pi" "Ya"]) => [[1 :a "You"] [2 :b "Pi"] [3 :c "Ya"]]
  )

(defn values-counts [f coll]
  (->> coll (group-by f) (transform-vals count)))
(comment 
  (values-counts identity
                  [:a :b :a :a :c :b :a :d :a :c :a :c :c :c :b ]) => {:d 1, :c 5, :b 3, :a 6}
  (values-counts inc
                  [0 1 1 2 2 2 3 3 3 3]) => {4 4, 3 3, 2 2, 1 1})

(defn most-frequent [coll]
  (let [frequencies (values-counts identity coll)]
    (argmax frequencies (keys frequencies))))
(comment
  (most-frequent
  [:a :a :b]) => :a
  (most-frequent
    [:a :a :a :b :b :c :b :b :a :b]) => :b
  (most-frequent
    [:a :a :a :b :b :c :b :b :a :a :a :b]) => :a
  )

(defn constant-coll? [coll]
  (if (empty? coll) 
    true
    (let [first-item (first coll)] (every? #(= % first-item) (rest coll)))
    ))
(comment 
  (constant-coll? ()) => true
  (constant-coll? [:a :a :a]) => true
  (constant-coll? [:a :b :a]) => false
  )

