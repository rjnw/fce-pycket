#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))

(letrec ((prime (lambda (n k)
                  (if (= k n)
                      true
                      (if (= (remainder n k) 0)
                          false
                          (prime n (+ k 1)))))))

  (let ((f (lambda () (prime 9887 2))))
    (begin (rec f 4)
           (time (rec f 1)))))
