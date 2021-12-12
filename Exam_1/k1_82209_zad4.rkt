#lang racket
(require math/number-theory)

(define (find-product n)
  (cond
    [(= n 0) 1]
    [else (* (remainder n 10) (find-product (quotient n 10)))]
    )
  )
(define (make-list n)
  (cond
    [(= (find-product (find-product n))  (find-product n)) (flatten (find-product n)) ]
    [else (flatten (cons (find-product n) (make-list (find-product n))))]
    )
  )


(define (persistence n)
  (cond
    [(< n 0) (error "N must be positive")]
    [else (cons (make-list n) (length (make-list n)))]
  )
  )

(persistence 39); → '((27 14 4) . 3) ; 3*9=27, 2*7=14, 1*4=4
(persistence 126); → '((12 2) . 2) ; 1*2*6=12, 1*2=2
(persistence 4); → '((4) . 1)
(persistence 999); → '((729 126 12 2) . 4)
;(persistence -56); → error
  
  