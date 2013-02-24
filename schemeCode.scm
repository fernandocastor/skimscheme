(begin
  (define f 10)
  (let ((g 3)) (+ f (set! g (+ f g))))
  f
)