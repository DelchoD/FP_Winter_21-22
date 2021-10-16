#lang racket

;Task_11
#|A cubic prime number is a prime number that is the
 difference between two subsequent natural numbers.
For example, 61 is such a number because 61 = 5^3 - 4^3.
Define a linearly-recursive procedure that returns the
 nth cubic prime number.
|#

(require math/number-theory)

(define (nth-cubic n)
  (define (helper min max number thTime)
    (cond
      [(<= n 0) (error "Vales can not be negative")]
      [(= thTime n) (-(expt (sub1 max) 3) (expt (sub1 min) 3))]
      [(prime? (-(expt max 3) (expt min 3))) (helper (add1 min) (add1 max) (-(expt max 3) (expt min 3)) (add1 thTime))]
      [else (helper (add1 min) (add1 max) (-(expt max 3) (expt min 3))  thTime)])
    )
  (helper 1 2 0 0)
  )




(= (nth-cubic 1) 7)
(= (nth-cubic 4) 61)
(= (nth-cubic 50) 55897)
(= (nth-cubic 100) 283669)
(= (nth-cubic 200) 1570357)
;(nth-cubic 0) ; should return an error