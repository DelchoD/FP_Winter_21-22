#lang racket

;Task_3
#|Define a recursive procedure that returns
the sum of all prime divisors of a given number.
|#

(require math/number-theory)

(define (sum-prime-divs-rec n)
  (define (helper sum currNum)
    (cond
      [(= currNum 1) sum ]
      [(and(divides? currNum n) (prime? currNum)) (helper (+ sum currNum)(sub1 currNum))]
      [else (helper sum (sub1 currNum))]
      )

    )
  (cond
    [(zero? n) 0]
    [(negative? n) (error "n was not positive")]
    [else (helper 0 n)]
  )
  )

(= (sum-prime-divs-rec 0) 0)
(= (sum-prime-divs-rec 6) 5) ; 2 + 3
(= (sum-prime-divs-rec 18) 5) ; 2 + 3
(= (sum-prime-divs-rec 19) 19)
(= (sum-prime-divs-rec 45136) 53)



