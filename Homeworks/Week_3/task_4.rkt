#lang racket

;Task_4
#|Define a recursive procedure (sum-divisible-numbers start finish k)
that returns the sum of all numbers from the interval [start, finish]
whose digits sum up to a number that is evenly divisible by k.
|#

(require math/number-theory)

(define (sum-digits-rec n)
  (cond
    [(< n 0) (error "n was not positive")]
    [(< n 10) n]
    [else (+ (remainder n 10) (sum-digits-rec (quotient n 10)))]
  )
  )
(define (sum-divisible-numbers start finish k)
  (define (helper min max sum)
    (cond
      [(= k 0) (error "we can not divide by zero")]
      [(> min max) sum]
      [(divides? k (sum-digits-rec min))(helper (add1 min) max (+ sum min))]
      [else (helper (add1 min) max sum)]
      )
    )
(if (> start finish)
    (helper finish start 0)
      (helper start finish 0))
  )

(= (sum-divisible-numbers 0 10 5) 5)
(= (sum-divisible-numbers 0 100 5) 990)
(= (sum-divisible-numbers 100 0 5) 990)

; Do we need to handle the case where k is negative