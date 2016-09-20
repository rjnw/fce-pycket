#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))

(letrec ((fib (lambda (n)
                (if (< n 2)
                    n
                    (+ (fib (- n 1))
                       (fib (- n 2)))))))
  (let ((f (lambda () (let ((env (capture-environment)))
                        (with-environment (fib 10) env)))))
    (begin (rec f 1000)
           (time (rec f 500000)))))

