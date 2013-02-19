(ns equations.utils  
  (:use [clojure.core.logic])
  (:refer-clojure :exclude [==]))

(defna simplifico [original result]
  ([[:sum 0 term1] res] (simplifico term1 res))
  ([[:sum term1 0] res] (simplifico term1 res))
  ([[:mul 0 term1] 0])
  ([[:mul term1 0] 0])
  ([[oper term1 term2] simplified-res] 
    (fresh [simp1 simp2]
          (simplifico term1 simp1) 
          (simplifico term2 simp2)
          (== simplified-res [oper simp1 simp2])
          ))
  ([term term]))

(defn simplify [expression]
  (loop [original expression 
         simplified (first (run* [q] (simplifico expression q)))]
    (if (= simplified original)
      simplified
      (recur simplified (first (run* [q] (simplifico simplified q)))))))