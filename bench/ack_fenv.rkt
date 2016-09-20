#lang racket
(define rec (letrec ((rec (lambda (f z)
                        (if (zero? z) (f) (begin (f) (rec f (- z 1)))))))
              rec))

(letrec ((ack (lambda (m n)
                (if (zero? m)
                    (+ n 1)
                    (if (zero? n)
                        (ack (- m 1) 1)
                        (ack (- m 1) (ack m (- n 1))))))))
  (let ((f (lambda () (let ((env (capture-environment)))
                        (with-environment (ack 3 9) env)))))
    (begin (rec f 5)
           (time (rec f 10)))))