#lang racket
;;; FIBC -- FIB using first-class continuations, written by Kent Dybvig

(define _1+ (lambda (n) (+ n 1)))
(define _1- (lambda (n) (- n 1)))

;;; fib with peano arithmetic (using numbers) with call/cc

(define addc
  (letrec ((addc (lambda (x y k)
                   (if (zero? y)
                       (k x)
                       (addc (_1+ x) (_1- y) k)))))
    addc))

(define fibc
  (letrec ((fibc (lambda (x c)
                   (if (zero? x)
                       (c 0)
                       (if (zero? (_1- x))
                           (c 1)
                           (addc (call-with-current-continuation (lambda (c) (fibc (_1- x) c)))
                                 (call-with-current-continuation (lambda (c) (fibc (_1- (_1- x)) c)))
                                 c))))))
    fibc))

(time (display (fibc 18 (lambda (c) c))))


