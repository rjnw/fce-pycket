#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))

(letrec ((fib (lambda (n)
                (let ((env (capture-environment)))
                  (with-environment
                    (if (< n 2)
                       n
                       (+ (fib (- n 1))
                          (fib (- n 2)))) env)))))
  (let ((f (lambda () (fib 10))))
    (begin (rec f 1000)
           (time (rec f 500000)))))

