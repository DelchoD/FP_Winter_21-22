#lang racket

;Task_4
#|Define a procedure that reverses a list using foldl.|#

(define (my-reverse-foldl xs)
    (foldl cons '() xs)) 

(equal? (my-reverse-foldl '(1 2 3 4 5)) '(5 4 3 2 1))
(equal? (my-reverse-foldl '(1 3 3 3 5)) '(5 3 3 3 1))
(equal? (my-reverse-foldl '(5)) '(5))
(equal? (my-reverse-foldl '()) '())
