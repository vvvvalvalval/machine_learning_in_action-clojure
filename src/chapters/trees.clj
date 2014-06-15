(ns chapters.trees
  (:require [utils.algo :as algo]
            [utils.collect :as ct]
            
            [clojure.java.io :as io]
            
            [clojure.string :as string]
            )
  (:use clojure.repl
        clojure.pprint)
  )

(defn create-dataset []
  {:labels [:no-surfacing, :flippers]
   :dataset [[1 1 :yes]
             [1 1 :yes]
             [1 0 :no]
             [0 1 :no]
             [0 1 :no]]})

(def calc-Shannon-entropy 
  (let [lg2 (java.lang.Math/log 2)
        -xlogx (fn [x] (* (- x) (/ (java.lang.Math/log x) lg2)))]
    (fn [get-label dataset]
     (let [label-counts (algo/values-counts get-label dataset)
           num-entries (count dataset)]
       (->> label-counts vals
         (map (fn [cpt] (-> cpt double (/ num-entries) -xlogx)) ) ; getting the log-probabilities
         (reduce + 0.0) ; adding them
         )))
    ))
(comment 
  (calc-Shannon-entropy #(nth % 2)
                        [[1 1 :yes]
                         [1 1 :yes]
                         [1 0 :no]
                         [0 1 :no]
                         [0 1 :no]]) => 0.9709505944546686)

(defn- split-on  [feat-idx dataset]
  (->> dataset (group-by #(nth % feat-idx) ) 
    (algo/transform-vals vec)))

(defn choose-best-feature-to-split [get-label features-indices dataset]
  (let [size (count dataset)
        
        base-entropy (calc-Shannon-entropy get-label dataset)
        entropy-of-split (fn [split] 
                           (reduce (fn [ent sub-dataset]
                                    (let [prob (-> sub-dataset count double (/ size))]
                                      (+ ent (* prob (calc-Shannon-entropy get-label sub-dataset)))
                                      ))
                                  0.0 (vals split)))
        info-gain-on-feat (fn [feat-idx] (- base-entropy
                                            (entropy-of-split (split-on feat-idx dataset)) ))
        ]
    (algo/argmax info-gain-on-feat features-indices)
    ))
(comment 
  (choose-best-feature-to-split 
    #(nth % 2) #{0 1}
    (:dataset (create-dataset))) => 0
 )

(def create-tree
  (let [aux (fn self[get-class labels dataset available-features-indices]
              (let [classes (map get-class dataset)]
                (cond
                  (algo/constant-coll? classes) (first classes), ; if unanim, return class value
                  (empty? available-features-indices) (algo/most-frequent classes), ; no more features to split on, majority vote
                  :else ; otherwise, keep on spliting
                  (let [best-feat-idx (choose-best-feature-to-split get-class available-features-indices dataset)
                        best-feat (labels best-feat-idx)
                        split-datasets (split-on best-feat-idx dataset)
                        remaining-indices (disj available-features-indices best-feat-idx)
                        children (algo/transform-vals
                                   (fn [sub-dataset]
                                     (self get-class labels sub-dataset remaining-indices)) ; recursive call
                                   split-datasets)]
                    [best-feat children])
                  )))]
    (fn [get-class labels dataset]
      (aux get-class labels dataset 
           (->> labels count range set))))
  )
(comment
  (let [{:keys [dataset labels]} (create-dataset)]
    (create-tree last labels dataset)) => [:no-surfacing {1 [:flippers {1 :yes, 0 :no}], 0 :no}]
)

(defn classifier-from [labels decision-tree]
  (let [index-for-label (reduce (fn [m i] (assoc m (labels i) i)) {} (range (count labels)))
        get-feature (fn [label instance] (instance (index-for-label label)))
        classify-with (fn [tree instance]
                        (if (vector? tree)
                          (let [feature (tree 0)]
                            (recur (get-in tree [1 (get-feature feature instance)]) instance))
                          tree))]
    (fn classify [instance]
      (classify-with decision-tree instance))
    ))
(comment 
  (let [classify (classifier-from [:no-surfacing, :flippers]
                                  [:no-surfacing {1 [:flippers {1 :yes,
                                                                0 :no}]
                                                  0 :no}])]
    (classify [1 0]) => :no
    (classify [1 1]) => :yes
    ))

