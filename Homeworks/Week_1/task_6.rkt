#lang racket

;Task_6
#|Define a procedure that reverses a non-negative number. |#


#|Can be made with:
    1. Recursion
    2. The Provisioned For "loop" in lang Racket
|#

#|Using Let for something like a declaration|#
(define (rev numberToReverse)
  (let helper ([reversedNumber 0]
          [numberToReverse numberToReverse])
    (cond
      [(zero? numberToReverse) reversedNumber]
      [else (helper (+ (* reversedNumber 10) (modulo numberToReverse 10)) (quotient numberToReverse 10))])))


(= (rev 1) 1)
(= (rev 123) 321)
(= (rev 987654321) 123456789)
(= (rev 5897) 7985)
(= (rev 22533) 33522)
(= (rev 51) 15)


