(ns chapters.trees
  (:require [utils.algo :as algo]
            [utils.collect :as ct]
            
            [clojure.java.io :as io]
            
            [clojure.string :as string]
            )
  (:use clojure.repl
        clojure.pprint)
  )

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