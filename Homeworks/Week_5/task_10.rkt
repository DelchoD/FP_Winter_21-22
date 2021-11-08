#lang racket

;Task_10
#|Define a procedure (insert-at x idx xs)
 that inserts an element at a current index.|#

(define (insert-at x idx xs)
  (cond
    [(> idx (length xs)) (error "The entered index is not valid")]
    [(= idx 0) (list* x xs)]
    [(= idx 1) (flatten (list* (car xs) (list x) (take-right xs (add1 idx))))]
    [(= idx (sub1 (length xs))) (flatten (list* xs x))]
    [else (flatten (list* (drop-right xs idx) (list x) (take-right xs  idx)))]
    )
  )
  
(equal? (insert-at 1 0 '()) '(1))
(equal? (insert-at 1 0 '(2)) '(1 2))
(equal? (insert-at 10 2 '(1 2 3 4)) '(1 2 10 3 4))
(equal? (insert-at 10 2 '(1 2 3)) '(1 2 3 10))