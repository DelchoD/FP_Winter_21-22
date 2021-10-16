#lang racket

;Task_12
#|Write a procedure that accepts three whole
numbers - a, b and nand returns the sum of the
last three numbers from the following sequence seen in the file "seq.png"

Note: n will always be > 3.
|#


(define (bracket-sum a b n)
  (define (helper currentNum total)
    (cond
      [(> currentNum n) total]
      [else (helper (+ currentNum 1) (+ (* (expt 2 (- currentNum 1)) b) total))]
      )
    )
  (helper 1 a)
  )

(define (find-sum a b n)
  (cond
    [(or (< n 3) (< a 0) (< b 0)) (error "n must be greater than 3 or positive")]
    [else (+ (bracket-sum a b n) (bracket-sum a b (- n 1)) (bracket-sum a b (- n 2)))]  
      )
  )

(= (find-sum 0 2 10) 3578) ; 510 + 1022 + 2046
(= (find-sum 5 3 5) 174) ; 26 + 50 + 98
(= (find-sum 2 7 4) 181)
;(= (find-sum -5 3 5) 174)
;(= (find-sum 5 -3 5) 174)
;(= (find-sum 5 3 -5) 174)
