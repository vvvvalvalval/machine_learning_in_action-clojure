(ns chapters.kNN
  (:require [utils.lang :as lu]
            [utils.algo :as algo]
            
            [incanter.core :as in]
            )
  (:use clojure.repl
        clojure.pprint)
  )

(defn create-dataset []
  {:group [[1.0, 1.1]
           [1.0 ,1.0]
           [0, 0]
           [0, 0.1]]
   :labels [:a :a :b :b]})

(lu/defd [{:keys [group labels]} (create-dataset)] group labels)

(defn euclidean-distance [a b]
  (let [d (in/minus a b)]
    (in/sqrt (in/inner-product d d))))

(defn classify0 [inX dataset labels k]
  (let [dataset (vec dataset)
        indices (range 0 (count dataset))
        k-best-indices (algo/k-best (algo/scores-comparer 
                                      (fn [idx] (euclidean-distance inX (dataset idx)))) 
                                    k indices)
        votes-for-label (->> k-best-indices
                          (group-by labels)
                          (algo/transform-vals count))
        winning-label (algo/argmax votes-for-label (keys votes-for-label))]
    winning-label))
(comment
  (classify0 [0 0] group labels 3) => :b
  (classify0 [1.2 0.9] group labels 3) => :a
  )







