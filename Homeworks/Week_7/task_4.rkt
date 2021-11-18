#lang racket

;Task_4
#|Define a procedure (shuffle xs) that gets a list of
 2 * n elements in the form '(x 1 x 2 ... x n y 1 y 2 ... y n)
 and returns a list in the form '(x 1 y 1 x 2 y 2 ... x n y n).|#

(define (shuffle xs)
  (if (not (even? (length xs))) (error "The length of xs should be even") 0)
  (define ls (take xs (/ (length xs) 2)))
  (define rs (drop xs (/ (length xs) 2)))
  (define (helper ys zs iter)
    (cond
      [(null? zs) zs]
      [(odd? iter) (cons (car ys) (helper (cdr ys) zs (add1 iter)))]
      [else (cons (car zs) (helper ys (cdr zs) (add1 iter)))]
  )
    )
  (helper ls rs 1)
  )
  



(shuffle '(2 5 1 3 4 7)) ; -> '(2 3 5 4 1 7)
(shuffle '(1 2 3 4 4 3 2 1)) ; -> '(1 4 2 3 3 2 4 1)
(shuffle '(1 1 2 2)) ; -> '(1 2 1 2)
;(shuffle '(2 5 1 7 3 4 7)) ; -> error "The length of xs should be even"