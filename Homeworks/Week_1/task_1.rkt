#lang racket
;Task_1
#|Euclidean Algorithm using guards|#
;Pseudo code
#|function gcd(a, b)
    while b ≠ 0
        t := b
        b := a mod b
        a := t
    return a
|#


(define (my-gcd x y)
  (cond
    [(= x 0) y]
    [(= y 0) x]
    [else (my-gcd y (modulo x y))]
    )
  )

(= (my-gcd 5 13) 1)
(= (my-gcd 13 1235) 13)

