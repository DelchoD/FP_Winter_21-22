#lang racket

;Task_3
#|Define a procedure that accepts two real numbers and
calculates the n-th partial sum from the following sequence in 
|#


(require math/number-theory)

(define (geometric-progression iteration)
  (define (helper number start digit)
  (cond
    [(> start iteration) number]
    [else (helper (* number digit) (add1 start) (+ digit 2))]
    ))
  (helper 1 1 3)
  )

(define (calc-series-sum x n)
  (define (helper current-num iter)
    (cond
      [(> iter n) current-num]
      [else (helper (+
             (/
              (* (expt -2 (add1 iter)) (expt x (sub1 iter)))
              (geometric-progression iter)
              )current-num) (add1 iter))
             ]
      )
  )
  (helper 0 0)
  )




(calc-series-sum 1 0) ; -2
(calc-series-sum 1 1) ; -2/3
(calc-series-sum 1 2) ; -1 1/5
(calc-series-sum 1 3) ; -1 1/21
(calc-series-sum 1 4) ; -1 11/135
(calc-series-sum 1 5) ; -1 29/385
(calc-series-sum 1 6) ; -1 937/12285