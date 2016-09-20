#lang racket
(time (display
       (letrec ((ack (lambda (m n)
                       (if (zero? m)
                           (+ n 1)
                           (if (zero? n)
                               (ack (- m 1) 1)
                               (let ((x (ack m (- n 1))))
                                 (ack (- m 1) x)))))))
         (ack 3 9))))
