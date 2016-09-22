#lang racket
;;; SUM -- Compute sum of integers from 0 to 10000
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))
(define sum
  (letrec ((sum (lambda (i n)
                  (let ((env (capture-environment)))
                    (with (if (< i 0)
                              0
                              (sum (- i 1) (+ i n)))
                          env)))))
    sum))
 
(let ((f (lambda () (let ((env (capture-environment)))
                      (with-environment (sum 10000 0) env)))))
    (begin (rec f 1000)
           (time (rec f 100000))))

