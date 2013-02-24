<<<<<<< HEAD
(define a (if (< 2 3) (+ 2 a) (- 2 a)))
=======
(begin
  (define f 10)
  (let ((g 3)) (+ f (set! g (+ f g))))
  f
)
>>>>>>> 00cd060ae68fab2b7bb6f0a506a670adb766cb7b
