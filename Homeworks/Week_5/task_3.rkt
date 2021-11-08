#lang racket

;Task_3
#|Define a procedure set-union xs ys that takes
two sets of numbers and returns their union.
It (the union) must be sorted in ascending order!|#

(define (wrapper xs ys)
 (sort (cond
    [(null? ys) xs]
    [(member (car ys) xs) (wrapper xs (cdr ys))]
    [else (wrapper (cons (car ys) xs) (cdr ys))])
   < ) 
  )

(define (set-union xs ys)
  (if (< (length ys) (length xs))
      (wrapper xs ys)
      (wrapper ys xs)
         )
      )
(equal? (set-union '(1 3 5 7) '(5 7 13)) '(1 3 5 7 13))
(equal? (set-union '(5 7 13) '(1 3 5 7)) '(1 3 5 7 13))