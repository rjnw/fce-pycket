(((lambda (f) (lambda (y) ((f f) y)))
  (lambda (g) (lambda (x) (if (zero? x) 1 (* x ((g g) (- x 1)))))))
 5)
