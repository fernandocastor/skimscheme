(begin
	(if (eqv? '(1 'a 3) '(1 'b 3)) '(list-comp x (1 5 6 7 2 3 725 23 178) (* x 2) (eqv? (mod x 2) 0)) (quotient 10 3))
)