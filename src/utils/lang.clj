(ns utils.lang
  (:use clojure.pprint
        clojure.repl)
  )

(defmacro defd [bindings & syms]
  "defines the specified symbols from the binding form."
  (let [def-all (map (fn [s] `(def ~s ~s)) syms)]
    `(let ~bindings
       ~@def-all)))
(comment
  (defd [[a {b :b, {:keys [d e]} :c}]
         ["Hello", {:b "How", :c {:d "are", :e "you?"}}]
         ]
    b d e)
  (str b " " d " " e) => "How are you?"
  a => nil)