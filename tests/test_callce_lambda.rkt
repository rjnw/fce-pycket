(let ((fact (lambda (x env) (if (zero? x) 1 (* x ((with-environment fact env) (- x 1) env))))))
  (let ((env (capture-environment)))
    (display (fact 5 env))))
