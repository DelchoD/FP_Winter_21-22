#lang racket

;Task_9
#|Given a divisor d and a bound b, find the largest integer N, such that:

N is divisible by d
and N is less than or equal to b
and N is greater than 0.
|#

(require math/number-theory)

(define (max-multiple divisor bound)
  (define (helper d b number)
  (cond
    [(or (< d 0) (< b 0)) (error "Values can not be negaitive")]
    [(and (divides? d number) (<= number b) (> number 0)) number]
    [(or (not(divides? d number)) (> number b) (< number 0)) (helper d b (sub1 number))]
    [else (error "Such number doen not exist")]
    )
  )
  (helper divisor bound bound)
  )


(= (max-multiple 2 7) 6)
(= (max-multiple 3 10) 9)
(= (max-multiple 7 17) 14)
(= (max-multiple 10 50) 50)
(= (max-multiple 37 200) 185)
(= (max-multiple 7 100) 98)
;(= (max-multiple -6 100) 98)
