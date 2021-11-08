#lang racket

;Task_6
#|According to the fundamental theorem of arithmentics
every natural number greater than 2 can be expressed as
a product of prime numbers. Define a procedure that returns
the sorted list of prime factors of a natural number.|#
(require math/number-theory)

(define (expand-list xs)
  (define (helper iter)
  (cond
    [(= iter (cadr xs)) (car xs)]
    [else (cons (car xs) (helper (add1 iter)))]
    ))
  (flatten (helper 1))
  )

(define (my-factorize xs)
  (cond
    [(null? (cdr xs)) (expand-list (car xs))]
    [else (cons (expand-list (car xs)) (my-factorize (cdr xs)))]
    )
  )

(define (wrapper number)
  (cond
    [(<= number 1) (error "This is not a valid number")]
  [else (sort (flatten (my-factorize (factorize number))) <)]
  )
  )

(equal? (wrapper 2) '(2))
(equal? (wrapper 6) '(2 3))
(equal? (wrapper 13) '(13))
(equal? (wrapper 123) '(3 41))
(equal? (wrapper 152) '(2 2 2 19))
(equal? (wrapper 12356498) '(2 7 11 19 41 103))
;(wrapper 1)