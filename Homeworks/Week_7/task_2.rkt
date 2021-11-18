#lang racket

;Task_2
#|Define a procedure that accepts a natural number n
and returns an unary function of k which returns a list
of the numbers between 1 and n (inclusive) such that no
number contains k or more prime divisors.|#

(require math/number-theory)

(define (numbers n)
  (λ (k)
    (cond
      [(not (natural? n)) (error "N is not natural")]
      [(= n 0) '()]
      [(> k (length (prime-divisors n))) (sort (list* n ((numbers (sub1 n)) k)) <)]
      [(prime? n) (sort (list* n ((numbers (sub1 n)) k)) <)]
      [else ((numbers (sub1 n)) k)]
      )
    )
  )

(equal? ((numbers 10) 1) '(1 2 3 5 7))
(equal? ((numbers 20) 2) '(1 2 3 4 5 7 8 9 11 13 16 17 19))
