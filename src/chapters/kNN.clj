(ns chapters.kNN
  (:require [utils.lang :as lu]
            [utils.algo :as algo]
            [utils.collect :as ct]
            
            [incanter.core :as in]
            [incanter.charts :as charts]
            
            [clojure.java.io :as io]
            [incanter.io :as inio]
            
            [clojure.string :as string]
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

;; GENERIC kNN CLASSIFICATION
(let [square #(* % %)]
  (defn euclidean-distance "faster euclidian distance" [a b]
   (loop [squares-sum 0.0
          rem-a a, rem-b b]
     (if rem-a
       (recur (+ (square (- (first rem-a) (first rem-b))) squares-sum)
              (next rem-a) (next rem-b))
       (java.lang.Math/sqrt squares-sum)))))

(defn classify0 [inX dataset labels k]
  (let [indices (range 0 (count dataset))
        k-best-votes (let [votes (mapv
                                   (fn [point label]
                                     {:label label :distance (euclidean-distance inX point)})
                                   dataset labels)]
                       (algo/k-lowest :distance k votes))
        votes-for-label (->> k-best-votes
                          (map :label)
                          (group-by identity)
                          (algo/transform-vals count))
        winning-label (algo/argmax votes-for-label (keys votes-for-label))]
    winning-label))
(comment
  (classify0 [0 0] group labels 3) => :b
  (classify0 [1.2 0.9] group labels 3) => :a
  )

;; DATING EXAMPLE

(let [dating-testset (ct/simple-read-CSV "datingTestSet.txt" \tab)]
  (def dating-labels (->> dating-testset (map #(get % 3)) vec))
  (def dating-group (->> dating-testset (map #(subvec % 0 3)) vec))
  
  (defn classify-date [k inX]
    (classify0 inX dating-group dating-labels k))
  )

(comment 
  "viewing the data"
  (in/view (charts/scatter-plot
           (map #(get % 0) dating-group)
           (map #(get % 1) dating-group)
           :x-label "Frequent Flyer Miles Earned per Year"
           :y-label "Percentage of time playing video games"
           :group-by dating-labels
           :legend true))
  )

;; DIGITS EXAMPLE

(def to-digit-vector "Converts a rectangular text image into a flat vector of doubles."
  (let [read-digit-line (fn [line] (map #(-> % str java.lang.Double/parseDouble) line))]
    (fn [^String digits-text-map]
     (->> digits-text-map
       string/split-lines
       (map read-digit-line)
       flatten vec))
    ))
(comment 
  (to-digit-vector"
001
101
110") => [0.0 0.0 1.0 1.0 0.0 1.0 1.0 1.0 0.0]
  )
(def training-digits-path "trainingDigits")
(def test-digits-path "testDigits")

(def read-digits-dataset 
  (let [find-digit (fn [^java.io.File file] (->> file .getName first))]
    (fn [dir-name]
      (let [files (ct/files-under-dir dir-name)] 
        {:group (->> files (map slurp) (map to-digit-vector) doall),
        :labels (->> files (map find-digit) doall)}
        ))
    ))
(do 
  (def fetch-training-dataset #(let [res (read-digits-dataset training-digits-path)]
                                 (println "Done loading the digits training set") res))
  (def fetch-test-dataset #(let [res (read-digits-dataset test-digits-path)]
                                 (println "Done loading the digits test set") res)))

(defn handwriting-class-test [k]
  (let [{tr-group :group, tr-labels :labels} (fetch-training-dataset),
        {ts-group :group, ts-labels :labels} (fetch-test-dataset),
        q&a (algo/zip ts-group ts-labels)
        [error-count total :as stats] 
        (reduce
          (fn [[err tot mismatches] [inX expected-class]]
            (let [proposed-class (classify0 inX tr-group tr-labels k)
                  correct (= proposed-class expected-class)]
              (println (str tot " : " 
                            "the classifier came back with " proposed-class ", the real answer is " expected-class))
              [(if correct err (inc err)), 
               (inc tot), 
               (if correct mismatches (conj mismatches {:expected expected-class, :proposed proposed-class}))]))
          [0 0 []] q&a)]
    {:errors (stats 0)
     :total (stats 1)
     :errors-details (stats 2)}))
(comment )



