(ns equations.diferentiate  
  (:use [clojure.core.logic])
  (:refer-clojure :exclude [==]))

;(defnu termo? [col result]
;  ([input output] (fresh [temp]
;                     (membero temp input)
;                     (== false output)
;                         ))
;  ([input true])
;  )

(defna derivo [expression variable equation] 
  ([[:sum term1 term2] var1 [:sum d1 d2]] (derivo term1 variable d1) (derivo term2 variable d2))
  ([[:mul term1 term2] var1 [:sum [:mul term1 d2] [:mul d1 term1]]] (derivo term1 variable d1) (derivo term2 variable d2))
  ([var1 var1 1])
  ([var1 var2 0] (!= var1 var2)))

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

(defn differentiate [expression variable]
  (run* [q]
      (derivo expression variable q)))

(defn simplify [expression]
  (loop [original expression 
         simplified (first (run* [q] (simplifico expression q)))]
    (if (= simplified original)
      simplified
      (recur simplified (first (run* [q] (simplifico simplified q)))))))

(simplify (first (differentiate [:sum [:mul 9 :X] [:mul 5 :Y]] :X)))
