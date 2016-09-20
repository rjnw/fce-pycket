#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))

(let ((fib (lambda (env n)
             (let ((fib (with-environment fib env)))
               (if (< n 2)
                   n
                   (+ (fib env (- n 1))
                      (fib env (- n 2))))))))
  (let ((f (lambda () (let ((env (capture-environment)))
                        (fib env 10)))))
    (begin (rec f 1000)
           (time (rec f 500000)))))

