#lang racket

;Task_1
#|Define a procedure that accepts an unary procedure f and a
 number y and returns an unary procedure that for every x returns:

y, if it is greater than or equal to the result of applying f to x;
the result of applying f to x, otherwise.
|#

(define (upper-bound f y)
  (λ (x)(max (f x) y))
  )
                              ;y   ;x
((upper-bound (λ (x) (* 2 x)) 101) 50)
(= ((upper-bound (λ (x) (* 2 x)) 100) 50) 100)
(= ((upper-bound (λ (x) (* 2 x)) 100.236) 500.002) 1000.004)
(= ((upper-bound (λ (x) (* 2 x)) 70) 30) 70)

