(begin
  (define a (list-comp x (1 2 3 4 5 6 7 8 9 10) (let ((a 2) (b 3)) (+ x a b)) (eqv? (mod x 2) 0)))
  a
)
