(time (display
       (letrec ((ack (lambda (m n)
                       (if (zero? m)
                           (+ n 1)
                           (if (zero? n)
                               (ack (- m 1) 1)
                               (ack (- m 1) (ack m (- n 1))))))))
         (ack 3 9))))
