#lang racket

(define (rev numberToReverse)
  (define (helper numberToReverse reversedNumber)
    (if (= numberToReverse 0)
        reversedNumber
        (let ((lastDigit (modulo numberToReverse 10)))
          (helper (/ (- numberToReverse lastDigit) 10) (+ (* reversedNumber 10) lastDigit)))))
  (helper numberToReverse 0))