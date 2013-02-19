(ns equations.diferentiate  
  (:use [clojure.core.logic])
  (:use [equations.utils])
  (:refer-clojure :exclude [==]))

(defna derivo [expression variable equation] 
  ([[:pow term1 term2] term2 [:mul term1 [:pow term2 [:diff term1 1]]]])
  ([[:sum term1 term2] var1 [:sum d1 d2]] (derivo term1 variable d1) (derivo term2 variable d2))
  ([[:mul term1 term2] var1 [:sum [:mul term1 d2] [:mul d1 term1]]] (derivo term1 variable d1) (derivo term2 variable d2))
  ([var1 var1 1])
  ([var1 var2 0] (!= var1 var2)))

(defn differentiate [expression variable]
  (run* [q]
      (derivo expression variable q)))

(simplify (first (differentiate [:sum [:mul 4 [:pow 2 :X]] [:mul 5 :Y]] :X)))
