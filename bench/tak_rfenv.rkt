#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))

(letrec ((tak (lambda  (env x y z)
                (let ((tak (with-environment tak env)))
                  (if (not (< y x))
                           z
                           (tak env
                                (tak env (- x 1) y z)
                                (tak env (- y 1) z x)
                                (tak env (- z 1) x y)))))))
  (let ((f (lambda () (let ((env (capture-environment)))
                        (tak env 18 12 6)))))
    (begin (rec f 10)
           (time (rec f 500)))))

