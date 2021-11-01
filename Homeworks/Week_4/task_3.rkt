#lang racket

;Task_3
#|Define a procedure that takes a single
argument procedure and returns it applied n times.
|#

(define (apply-n func times)
  (λ (x)
    (cond
      [(= times 1) (func x)]
      [else ((apply-n func (- times 1)) (func x))]
    )
  )
)

(= ((apply-n (λ (x) (* 2 x)) 5) 2) 64)
(= ((apply-n (λ (x) (quotient x 10)) 2) 100) 1)


