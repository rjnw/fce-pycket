(let ((z 500000))
  (fix ((rec (lambda (z) (if (zero? z) 0 (rec (- z 1))))))
       (rec z)))
