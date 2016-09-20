#lang racket
(time
 (display
  (let ((y 20))
    (letrec ((fact (lambda (x) (if (zero? x) 1 (* x (fact (- x 1)))))))
      (let ((z 5000))
        (letrec ((rec (lambda (z)
                        (if (zero? z) (fact y) (begin (rec (- z 1)) (fact y))))))
          (rec z)))))))
