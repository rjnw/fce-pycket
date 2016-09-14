#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin0 (f) (rec f (- z 1)))))))
              rec))

(letrec ((fib (lambda (n)
                (if (< n 2)
                    n
                    (+ (fib (- n 1))
                       (fib (- n 2)))))))
  (let ((f (lambda () (fib 10))))
    (time (rec f 500))))

