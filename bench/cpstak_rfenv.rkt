#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))
(letrec
    ((tak (lambda (env x y z k)
            (if (not (< y x))
                (k z)
                (tak env
                     (- x 1)
                     y
                     z
                     (lambda (v1)
                       (tak env
                            (- y 1)
                            z
                            x
                            (lambda (v2)
                              (tak env
                                   (- z 1)
                                   x
                                   y
                                   (lambda (v3)
                                     (tak env v1 v2 v3 k)))))))))))
  (let ((f (lambda () (let ((env (capture-environment)))
                        (tak env 18 12 6 (lambda (x) x))))))
    (begin (rec f 50)
           (time (rec f 1000)))))
