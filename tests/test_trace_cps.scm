(display
 (let ((y 20))
   (letrec ((fact (lambda (x) (if (zero? x) 1
                                  (let ((fx (fact (- x 1))))
                                    (* x fx))))))
        (let ((z 5000))
          (letrec ((rec (lambda (z)
                       (if (zero? z) (fact y) (begin0 (rec (- z 1)) (fact y))))))
               (time (rec z)))))))
