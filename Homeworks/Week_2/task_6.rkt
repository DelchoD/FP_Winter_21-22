#lang racket

;Task_6
#|Define a procedure sum-special-primes n d that returns the
 sum of the first n prime numbers that contain a digit d.
|#

(require math/number-theory)

(define (count-occurences number digit)
  (define (helper currentNum total)
  (cond
    [(and (< currentNum 10) (= currentNum digit)) (add1 total)]
    [(and (< currentNum 10) (not(= currentNum digit))) total]
    [(= (remainder currentNum 10) digit) (helper (quotient currentNum 10) (add1 total))]
    [else (helper (quotient currentNum 10) total)]
  )
  )
  (cond
    [(or (<= number 0) (<= digit 0)) (error "Values can not be negative")]
    [else (helper number 0)]
  )
  )

(define (sum-special-primes n d)
  (define (mainHelper currentN sum isPrime)
    (cond
      [(= currentN n) sum]
      [(and (prime? isPrime) (> (count-occurences isPrime d) 0)) (mainHelper (add1 currentN) (+ sum isPrime) (add1 isPrime))]
      [else (mainHelper currentN sum (add1 isPrime))]
      )
    )
  (mainHelper 0 0 2)
  )

(= (sum-special-primes 5 2) 392)
(= (sum-special-primes 5 3) 107)
(= (sum-special-primes 10 3) 462)