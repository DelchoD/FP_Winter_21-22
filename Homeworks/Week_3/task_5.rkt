#lang racket

;Task_5
#|A number - x, is a pentagonal number if we can
plot x poits in the form of a pentagon on a plain.
A number - x, is a pentagonal number if we can plot
x poits in the form of a pentagon on a plain.
|#

(define (p n)
  (define (helper iter number)
    (cond
      [(> iter n) number]
      [else (helper (add1 iter) (/ (- (* 3 iter iter) iter) 2))]
      )
    )
  (helper 1 0)
  )

(= (p 1) 1)
(= (p 2) 5)
(= (p 3) 12)
(= (p 4) 22)
(= (p 5) 35)
(= (p 6) 51)