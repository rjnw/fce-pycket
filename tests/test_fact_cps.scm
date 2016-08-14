;#lang racket
(time
 (display
  (let ((y 20))
    (letrec ((fact (lambda (x k) (if (zero? x) (k 1) (fact (- x 1) (lambda (v) (k (* x v))))))))
      (let ((z 5000))
        (letrec ((rec (lambda (z)
                        (if (zero? z)
                            (fact y (lambda (x) x))
                            (begin0 (rec (- z 1))
                                    (fact y (lambda (x) x)))))))
          (rec z)))))))
