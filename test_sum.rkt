(time
 (let ((z 5000000))
   (letrec
    ((rec (lambda (z)
            (if (zero? z)
                0
                (rec (- z 1))))))
    (rec z))))
