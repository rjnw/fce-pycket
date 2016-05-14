(((lambda (f) (lambda (x) ((f f) x)))
  (lambda (f) (lambda (x) (if (zero? x) 1 (* x ((f f) (- x 1)))))))
 5)
