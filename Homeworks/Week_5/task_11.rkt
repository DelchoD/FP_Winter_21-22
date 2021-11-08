#lang racket

;Task_11
#|Define a procedure that concatenates two lists.|#

(define (concat-proc xs ys)
  (flatten (list* xs ys))
  )
; using a predefined procedure
(equal? (concat-proc '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))

(define (concat-rec xs ys)
  (cond
    [(null? ys) (flatten (list xs))]
    [else (concat-rec (list xs (car ys)) (cdr ys))]
    )
  )
; using a linearly recursive process
(equal? (concat-rec '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))