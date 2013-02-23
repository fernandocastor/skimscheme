(begin 
  (define f 
    (lambda (x) (+ x 10))
  ) 
  (define result 
    (f (car '(50 34 567 433 22 23 2345 "ok" (6 87 6))))
  ) 
  result
)