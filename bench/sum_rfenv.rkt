#lang racket
;;; SUM -- Compute sum of integers from 0 to 10000
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))
(define sum
  (lambda (env i n)
    (let ((sum (with-environment sum env)))
      (if (< i 0)
          0
          (sum env (- i 1) (+ i n))))))

(let ((f (lambda () (let ((env (capture-environment)))
                      (sum env 10000 0)))))
    (begin (rec f 1000)
           (time (rec f 100000))))

