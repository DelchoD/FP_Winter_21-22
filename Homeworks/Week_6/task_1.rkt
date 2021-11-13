#lang racket

;Task_1
#|Define a procedure that accepts a list of numbers
and a list of predicates and returns only the elements
that satisfy all of the predicates..|#

(define (where xs preds)
  (cond
    [(null? preds) xs]
    [else (filter (car preds) (where xs (cdr preds)))]
    )
  )

(equal? (where '(3 4 5 6 7 8 9 10) (list even? (lambda (x) (> x 5)))) '(6 8 10)) ; all even numbers greater than 5
(equal? (where '(3 4 5 7) (list even? (lambda (x) (> x 5)))) '()) ; no numbers are even and greater than 5
