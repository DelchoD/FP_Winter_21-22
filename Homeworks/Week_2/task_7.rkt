#lang racket

;Task_7
#|Define a procedure that returns the number
of occurrences of a digit in a positive number.
|#



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

(= (count-occurences 121 1) 2)
(= (count-occurences 121 5) 0)
;(= (count-occurences -121 1) 2)
;(= (count-occurences 121 -1) 2)

