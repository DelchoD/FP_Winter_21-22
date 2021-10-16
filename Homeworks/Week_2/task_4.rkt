#lang racket

;Task_4
#|Define a recursive procedure
returning the number of palindromes in the interval [a, b].
|#

(define (rev n)
  (define (helper result left-over)
    (if (zero? left-over)
        result
        (helper (+ (* result 10) (remainder left-over 10)) (quotient left-over 10))
        )
    )
  (helper 0 n)
  )

(define (palindrome? n)
  (= n (rev n))
  )

(define (num-palindromes-recursion a b)
  (cond [(> a b)         0]
        [(palindrome? a) (+ 1 (num-palindromes-recursion (add1 a) b))]
        [else            (num-palindromes-recursion (add1 a ) b)]))
(define (num-palindromes-rec min max)
        (cond
          [(or (< min 0) (< max 0)) (error"values can not negative")]
          [(<= min max) (num-palindromes-recursion min max)]
          [(> min max) (num-palindromes-recursion max min)]
          [else (error "Some error")]
          )
        )
(= (num-palindromes-rec 1 101) 19)
(= (num-palindromes-rec 1 100) 18)
(= (num-palindromes-rec 100 1) 18)
(= (num-palindromes-rec 10 10) 0)
;(= (num-palindromes-rec -100 1) 18)

(define (num-palindromes-iteration a b)
  (define (helper min max total)
  (cond [(> min max)         total]
        [(palindrome? min) (helper (add1 min) max (+ total 1))]
        [else            (helper (add1 min) max total )]))
  (helper a b 0)
  )
  (define (num-palindromes-iter min max)
        (cond
          [(or (< min 0) (< max 0)) (error"values can not negative")]
          [(<= min max) (num-palindromes-iteration min max)]
          [(> min max) (num-palindromes-iteration max min)]
          [else (error "Some error")]
          )
        )

(= (num-palindromes-iter 1 101) 19)
(= (num-palindromes-iter 1 100) 18)
(= (num-palindromes-iter 100 1) 18)
(= (num-palindromes-iter 100 1) 18)
(= (num-palindromes-iter 10 10) 0)
;(= (num-palindromes-iter -100 1) 18)