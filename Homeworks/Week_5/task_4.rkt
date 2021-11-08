#lang racket


;Task_4
#|Define a procedure that accepts a list of digits
 and returns the number that is build by traversing
 the list from right to left.|#

(define (wrapper-proc xs)
  (foldl (λ (x acc) (+ (* acc 10) x)) 0 xs)
  )

(define (rev-fold xs)
  (wrapper-proc (reverse xs)))


(define (wrapper-iter xs)
  (define (helper sum ys)
  (cond
    [(null? ys) sum]
    [else (helper (+ (* sum 10) (car ys)) (cdr ys))]
    )
    )
  (helper 0 xs)
  )

    (define (rev-lin-rec xs)
  (wrapper-iter (reverse xs)))

; using folding
(= (rev-fold '(1 2 3)) 321)
(= (rev-fold '(1 2 3 4 5 6 7 8 9)) 987654321)

; using a linearly recursive procedure
(= (rev-lin-rec '(1 2 3)) 321)
(= (rev-lin-rec '(1 2 3 4 5 6 7 8 9)) 987654321)