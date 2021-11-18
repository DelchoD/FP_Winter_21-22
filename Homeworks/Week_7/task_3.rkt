#lang racket

;Task_3
#|Some numbers have interesting properties. For example:
     89 → 8 1 + 9 2 = 89 * 1
     695 → 6 2 + 9 3 + 5 4 = 1390 = 695 * 2
     46288 → 4 3 + 6 4 + 2 5 + 8 6 + 8 7 = 2360688 = 46288 * 51
Define a procedure (dig-pow n p) that takes a natural number n
(written with repetitive numbers abcd...) and finds naturally
number k - such that (a p + b p + 1 + c p + 2 + d p + 3 + ...) = n * k. If the number k s
the specified property does not exist, return -1.|#

(define (num-to-xs x)
  (define (helper result left-over)
    (cond
      [(not (natural? left-over)) (error "N is not natural")]
      [(< left-over 10) (cons left-over result)]
      [else (helper (cons (remainder left-over 10) result) (quotient left-over 10))]
     )
    )
  (helper '() x)
  )
(define (get-product xs pow)
  (cond
    [(not (natural? pow)) (error "p is not natural")]
    [(null? xs) 0]
    [else (+ (expt (car xs) pow) (get-product (cdr xs) (add1 pow)))]
    )
  )

(define (dig-pow n p)
  (define digits (num-to-xs n))
  (cond
    [(= 0 (remainder (get-product digits p) n)) (quotient (get-product digits p) n)]
    [else -1]
  )
  )



(dig-pow 89 1) ; -> 1 (81 + 92 = 89 = 89 * 1)
(dig-pow 92 1) ; -> -1 (няма k - такова, че 91 + 22 = 92 * k)
(dig-pow 695 2) ; -> 2 (62 + 93 + 54 = 1390 = 695 * 2)
(dig-pow 46288 3) ; -> 51 (43 + 64 + 25 + 86 + 87 = 2360688 = 46288 * 51)
;(dig-pow 5 -3) ; -> error "p is not natural" #in fact it can be but we dont want :)
;(dig-pow -5 3) ; -> error "N is not natural"