#lang racket

;Task_1
#|Define a recursive and an iterative procedure for
calculating the number of digits of a non-negative number.
using guards.
|#

(require math/number-theory)


(define (count-digits-rec n)
  (cond
    [(zero? n) 0]
    [(not (positive? n)) (error "n was not positive")]
    [(< n 10) 1]
    [else (+ 1 (count-digits-rec (quotient n 10)))]
  )
  )

(define (count-digits-iter n)
  (define (helper result current-n)
    (if (< current-n 10)
        result
        (helper (+ result 1) (quotient current-n 10))
        )
    )
  (cond
    [(zero? n) 0]
    [(negative? n) (error "n was not positive")]
    [else (helper 1 n)]
  )
  )

(= (count-digits-rec 12345) 5)
(= (count-digits-rec 123) 3)
(= (count-digits-rec 0) 0)
;(= (count-digits-rec -12345) 5)

(= (count-digits-iter 12345) 5)
(= (count-digits-iter 123) 3)
(= (count-digits-iter 0) 0)
 ;(= (count-digits-iter -123) 3)