#lang racket

;Task_7
#|Define a procedure that accepts a list of numbers and returns an unary procedure of a natural number - k,
such that the result from a call to it (the new procedure) is the kth largest negative number in the list.|#

(define (clean-up xs)
    (filter negative? (sort xs >))
  )

(define (kth-max-min xs)
  (clean-up xs)
  (λ (x)
    (cond
           [(< (length (clean-up xs)) x) (error "No such number!")]
           [else (list-ref (clean-up xs) (- x 1))]  
    )
    )
  )

(= ((kth-max-min '(-1)) 1) -1)
(= ((kth-max-min '(-1 -5 -6 -6 -6 -6)) 2) -5)
(= ((kth-max-min '(1 2 3 4 -5 6 7 -2 -1 0)) 2) -2)
;((kth-max-min '(-1 0 -1 0 -2 3 1 -1)) 3) ; error: No such number!