(time
 (display
  (let ((y 20))
    (letrec ((fact (lambda (x tot) (if (zero? x) tot (fact (- x 1) (* x tot))))))
      (let ((z 5000))
        (letrec ((rec (lambda (z)
                        (if (zero? z) (fact y 1) (begin0 (rec (- z 1)) (fact y 1))))))
          (rec z)))))))
