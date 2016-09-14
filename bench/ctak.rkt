#lang racket
;;; CTAK -- A version of the TAK procedure that uses continuations.

(define ctak-aux
  (letrec ((ctak-aux (lambda (k x y z)
                    
                    (if (not (< y x))
                        (k z)
                        (call-with-current-continuation
                         (lambda (k)
                           (ctak-aux
                            k
                            (call-with-current-continuation
                             (lambda (k) (ctak-aux k (- x 1) y z)))
                            (call-with-current-continuation
                             (lambda (k) (ctak-aux k (- y 1) z x)))
                            (call-with-current-continuation
                             (lambda (k) (ctak-aux k (- z 1) x y))))))))))
    ctak-aux))

(define ctak (lambda ( x y z)
               (call-with-current-continuation
                (lambda (k) (ctak-aux k x y z)))))
(time (display (ctak 18 12 6)))

