#lang racket

;Task_2
#|Define a procedure that accepts a list of numbers
 and returns an unary procedure of a natural number - k,
 such that the result from a call to it (the new procedure)
 is the kth largest negative number in the list.|#

(define (longest-ascending-sub xs)
   (cond
     [(null? xs) (list (car xs) (longest-ascending-sub (cdr ys)))]
     [(< (car xs) (cadr xs)) (list (car xs) (longest-ascending-sub (cdr ys)))]
     [else (empty)]
     )
   )


(longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5))

(equal? (longest-ascending-sub '(1 0 5)) '(0 5))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 6 7 7 1 5)) '(1 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 3 1 5 2 7 7 15)) '(2 7 7 15))
(equal? (longest-ascending-sub '(1 5 2 3 4 5 6 7 7 1 5)) '(2 3 4 5 6 7 7))
(equal? (longest-ascending-sub '(1 5 2 4 6 8 3 4 1)) '(2 4 6 8))