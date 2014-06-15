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

(defn choose-best-feature-to-split [dataset & [num-features]]
  (let [num-features (or num-features (-> dataset first count (- 1)))
        get-label #(nth % num-features)
        size (count dataset)
        
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
    (algo/argmax info-gain-on-feat (range num-features))
    ))
(comment 
  (choose-best-feature-to-split
    (:dataset (create-dataset)) 2) => 0
  )