#lang racket

;Task_2
#|Define an iterative procedure for calculating
the sum of the digits of a non-negative number.
|#


(define (sum-digits-iter n)
  (define (helper sum currentNum)
    (if (< currentNum 10)
        (+ sum currentNum)
        (helper (+ sum (remainder currentNum 10)) (quotient currentNum 10))
        )
    )
  (cond
    [(zero? n) 0]
    [(negative? n) (error "n was not positive")]
    [else (helper 0 n)]
  )
  )

(= (sum-digits-iter 12345) 15)
(= (sum-digits-iter 123) 6)
(= (sum-digits-iter 2523) 12)
(= (sum-digits-iter 0) 0)
;(sum-digits-iter -13) ; error "n was negative"
