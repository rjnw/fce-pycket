#lang racket

(require redex)


(define-language fe-pycket
  [e ::= x number (e ...) s]
  [x ::= variable]
  [prim ::= + -]
  [s ::= lambda let + -]
  [v ::= x number (closure e e ρ)]
;  [C ::= hole (C e ...)]
  [ρ ::= () (x v ρ) (x s ρ)]

  [κ ::=
     (letk x e ρ κ)
     (lambdak x e ρ κ)
     (appk e ρ κ)
     (prim1k prim e ρ κ)
     (prim2k prim v κ)]

  [state ::= (e ρ κ)])


(define-metafunction fe-pycket
  ρ-lookup : ρ x -> v
  [(ρ-lookup () x) x]
  [(ρ-lookup (x v ρ) x) v]
  [(ρ-lookup (x_1 _ ρ)) (ρ-lookup ρ x)])

(define →
  (reduction-relation
   fe-pycket
   (--> (x ρ κ)
        (plug-reduce κ (ρ-lookup ρ x)))
   (--> ((e_1 e_2 ...) ρ κ)
        (e_1 ρ (appk (e_2 ...) ρ κ)))))


(define-metafunction fe-pycket
  plug-reduce : κ v -> state
  [(plug-reduce (letk x_1 e_2 ρ κ) v) (e_2 (x_1 v ρ) κ)]
  [(plug-reduce (lambdak x_1 e_2 ρ κ) v) (e_2 (x_1 v ρ) κ)]
  [(plug-reduce (prim1k prim e ρ κ) v) (e ρ (prim2k prim v κ))]
  [(plug-reduce (prim2k prim v_1 κ) v_2) (plug-reduce κ (prim-δ prim v_1 v_2))]
  [(plug-reduce (appk e ρ κ) v) (δ v e ρ κ)])
