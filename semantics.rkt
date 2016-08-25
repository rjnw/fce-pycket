#lang racket

(require redex)

(define-language fe-pycket
  [e ::= x b (e ...) s]
  [b ::= number]
  [x ::= variable]
  [prim ::= + -]
  [s ::= lambda let + -]
  [v ::= b (closure x e ρ) s]
  [sv ::= s v]
  [ρ ::= () (x v ρ) (x s ρ)]

  [κ ::=
     (letk x e ρ κ)
     (lambdak x e ρ κ)
     (appk e ρ κ)
     (prim1k prim e ρ κ)
     (prim2k prim v κ)
     haltk]

  [state ::= (e ρ κ) (end v)])


(define-metafunction fe-pycket
  ρ-lookup : ρ x -> sv
  [(ρ-lookup () x) x]
  [(ρ-lookup (x v ρ) x) v]
  [(ρ-lookup (x_1 _ ρ)) (ρ-lookup ρ x)])

(define →
  (reduction-relation
   fe-pycket
   (--> (x ρ κ)
        (plug-reduce κ (ρ-lookup ρ x)))
   (--> (b ρ κ)
        (plug-reduce κ b))
   (--> ((e_1 e_2 ...) ρ κ)
        (e_1 ρ (appk (e_2 ...) ρ κ)))))


(define-metafunction fe-pycket
  plug-reduce : κ sv -> state
  [(plug-reduce (letk x_1 e_2 ρ κ) v) (e_2 (x_1 v ρ) κ)]
  [(plug-reduce (lambdak x_1 e_2 ρ κ) v) (e_2 (x_1 v ρ) κ)]
  [(plug-reduce (prim1k prim e ρ κ) v) (e ρ (prim2k prim v κ))]
  [(plug-reduce (prim2k prim v_1 κ) v_2) (plug-reduce κ (prim-δ prim v_1 v_2))]
  [(plug-reduce (appk e ρ κ) v) (δ v e ρ κ)]
  [(plug-reduce haltk v) (end v)])

(define-metafunction fe-pycket
  δ : sv e ρ κ -> state
  [(δ (closure x_1 e_2 ρ_1) (e_3) ρ κ) (e_3 ρ (lambdak x_1 e_2 ρ_1 κ))]
  [(δ lambda ((x_1) e_2) ρ κ) (plug-reduce κ (closure x_1 e_2 ρ))]
  [(δ let (((x_1 e_1)) e_2) ρ κ) (e_1 ρ (letk x_1 e_2 ρ κ))]
  [(δ + (e_1 e_2) ρ κ) (e_1 ρ (prim1k + e_2 ρ κ))]
  [(δ - (e_1 e_2) ρ κ) (e_1 ρ (prim1k - e_2 ρ κ))])

(define-metafunction fe-pycket
  prim-δ : prim b b -> b
  [(prim-δ + b_1 b_2) ,(+ (term b_1) (term b_2))]
  [(prim-δ - b_1 b_2) ,(- (term b_1) (term b_2))])

;(define init-env ()) ;;no need as the env-lookup is kinda funky and returns the symbol if not
;; found and we want to do the same in initial environment

;; (traces → (term ((+ 1 1) () haltk)))
;; (traces → (term ((+ 1 (- 10 9)) () haltk)))
;; (traces → (term (((lambda (x) x) 3) () haltk)))
;(traces → (term ((let ((x 42)) x) () haltk)))
(traces → (term (((lambda (x) (x ((y 42)) y)) let) () haltk)))
