#lang racket
(require math/number-theory)

(define (find-sum number)
  (if (< number 0)
      (error "Number must be positive")
      '()
      )
  (define (helper iter)
    (cond
      [(> (expt 5 iter) number) 0]
      [else (+ (floor (/ number (expt 5 iter))) (helper (add1 iter)))]
    )
  )
  (helper 1)
  )

(define (trailing-zeros n)
  (lambda (p) (p (find-sum n)))
  )


((trailing-zeros 6) even?); → #f (броят на влачещите нули е 1. 1 не е четно)
((trailing-zeros 1000) even?); → #f (броят на влачещите нули е 249. 249 не е четно)
((trailing-zeros 100000) even?); → #f (броят на влачещите нули е 24999. 24999 не е четно)
((trailing-zeros 1000000000) even?); → #t (броят на влачещите нули е 249999998. 249999998 е четно)
;((trailing-zeros -6) even?); → error