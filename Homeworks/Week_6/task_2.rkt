#lang racket

;Task_2
#|Define a procedure that reverses a list using foldr.|#


(define (my-reverse-foldr xs)
  (flatten (foldr (λ (y curr) (list curr y)) '() xs))
 )
(equal? (my-reverse-foldr '(1 2 3 4 5)) '(5 4 3 2 1))
(equal? (my-reverse-foldr '()) '())