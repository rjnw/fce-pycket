#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))

(letrec ((partitions (lambda (n m)
                       (if (= n 0)
                           1
                           (if (< n 0)
                               0
                               (if (= m 0)
                                   0
                                   (+ (partitions (- n m) m)
                                      (partitions n (- m 1)))))))))
  (let ((f (lambda () (partitions 50 50))))
    (time (rec f 100))))
