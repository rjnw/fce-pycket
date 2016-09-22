#lang racket
;;; SUM -- Compute sum of integers from 0 to 10000
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))
(define sum
  (letrec ((sum (lambda (i n)
                   (if (< i 0)
                       0
                       (sum (- i 1) (+ i n))))))
    sum))
 
(let ((f (lambda () (sum 10000 0))))
    (begin (rec f 1000)
           (time (rec f 100000))))

