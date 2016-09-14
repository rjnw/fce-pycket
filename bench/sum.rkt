#lang racket
;;; SUM -- Compute sum of integers from 0 to 10000

(define sum
  (letrec ((sum (lambda (i n)
                   (if (< i 0)
                       0
                       (sum (- i 1) (+ i n))))))
    sum))
 
(time (display (sum 10000 0)))

