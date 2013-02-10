(ns equations.core  
  (:use [clojure.core.logic])
  (:refer-clojure :exclude [==]))

(defrel operator op)
(defrel opposite op1 op2)

(defn opposing-ops [op1 op2]
  (fact operator op1)
  (fact operator op2)
	(fact opposite op1 op2)
	(fact opposite op2 op1)
  )

(opposing-ops :mul :div)
(opposing-ops :diff :sum)

(defne conmuto [before after]
  ([[op lb-t rb-t] _] 
    (conde
	    [(== after [op lb-t rb-t])]
	    [(== after [op rb-t lb-t])])))

(defne simplo [before after]
  ([[:equal [com op1 op2] r-t] _] 
    (fresh [com2]
      (opposite com2 com)
      (conde
        [(== after [:equal op1 [com2 r-t op2]])]
        [(== after [:equal op2 [com2 r-t op1]])]))))

(defn equationize [system]
	 (run* [q]
		(fresh [equ conmutada]
      (== equ system)
		  (conmuto equ conmutada)
		  (simplo conmutada q))) 
  )

(equationize [:equal [:mul :M :V] [:mul :Y :P]])

