;((lambda (x) x) 42)
(define x 42)
(define (id x) x)
(let ((lambda 5)
      (id (lambda (x) x))) (id lambda))

(letrec ((fact (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))
         (y 5)) (fact y))
