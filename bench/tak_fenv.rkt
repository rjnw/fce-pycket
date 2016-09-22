#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))

(letrec ((tak (lambda  (x y z)
                (let ((env (capture-environment)))
                  (if (not (< y x))
                           z
                           (tak (tak (- x 1) y z)
                                (tak (- y 1) z x)
                                (tak (- z 1) x y))) env))))
  (let ((f (lambda () (let ((env (capture-environment)))
                        (with-environment (tak 18 12 6) env)))))
    (begin (rec f 10)
           (time (rec f 500)))))

