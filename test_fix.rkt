(fix (fact (lambda (x) (if (zero? x) 1 (* x (fact (- x 1))))))
     (fact 5))
