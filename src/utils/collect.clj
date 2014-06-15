(ns utils.collect
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            
            [incanter.core :as in]
            [incanter.io :as incio])
  (:use clojure.repl
        clojure.pprint)
  )

(defn dataset-from-resource [r & opts]
  (apply incio/read-dataset (io/resource r) opts))

(defn simple-read-CSV [filename & [sep]]
  (let [sep (or sep \,),
        {:keys [column-names rows]} (-> filename io/resource 
                                      (incio/read-dataset :delim sep))
        as-vector (fn [row] (->> column-names (map row) vec))]
    (map as-vector rows)))

(defn autonorm [[first-row & _ :as rows]]
  (let [indices (range (count first-row))
        
        find-min (fn [i] (->> rows (map #(nth % i)) (reduce min)))
        find-max (fn [i] (->> rows (map #(nth % i)) (reduce max)))
        
        normalizers (vec (for [i indices]
                           (let [mn (find-min i), mx (find-max i)
                                 d (- mx mn)]
                             #(-> % (- mn) (/ d) double) 
                             )))
        normalize-row (fn [row] (vec (for [i indices] ((normalizers i) (row i)))))
        ]
    (->> rows (map normalize-row) vec)
    ))

(defn files-under-dir 
  "Fetches the sequence of files under the specified resource directory."
  [rsrc-dir]
  (->> rsrc-dir io/resource io/file file-seq 
    (drop 1) ; to remove the directory itself
    ))