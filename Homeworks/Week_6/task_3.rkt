#lang racket

;Task_3
#|Define a procedure that takes a list of numbers
 and returns a list of pairs in the form (xi . ni)
 where xi spans the elements of xs and ni is the number
 of elements in xs that are greater than xi.|#

  (define (larger ys el)
    (length (filter (λ (x) (> x el)) (sort ys <=)))
  )

(define (num-bigger-elements xs)
  (define temp xs)
  (define (helper xs)
    
(cond
  [(null? xs) xs]
  [else (list* (cons (car xs) (larger temp (car xs))) (helper (cdr xs)))]
 )
  )
  (helper xs )
  )


(equal? (num-bigger-elements '(5 6 3 4)) '((5 . 1) (6 . 0) (3 . 3) (4 . 2)))
(equal? (num-bigger-elements '(1 1 1)) '((1 . 0) (1 . 0) (1 . 0)))