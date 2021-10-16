#lang racket

;Task_8
#|A number is interesting if and only if it is evenly
divided by the sum of its digits. Define a predicate
that checks whether a number is interesting.
|#
(require math/number-theory)

(define (sum-digits-rec n)
  (cond
    [(not (positive? n)) (error "n was not positive")]
    [(< n 10) n]
    [else (+ (remainder n 10) (sum-digits-rec (quotient n 10)))]
  )
  )

(define (interesting? number)
  (if (divides? (sum-digits-rec number) number)
      #t
      #t
      )
  )

(equal? (interesting? 410) #t)

;Do we want the number to be even?