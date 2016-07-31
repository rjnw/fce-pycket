(time
 (let ((z 500000))
   (letrec
    ((rec (lambda (z)
            (if (zero? z)
                0
                (rec (- z 1))))))
    (rec z))))
