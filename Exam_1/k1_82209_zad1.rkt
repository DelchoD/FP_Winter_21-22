#lang racket
(require math/number-theory)

(define (make-list n)
 (cond
   [(= 0 n) '()]
   [else (cons (remainder n 10) (make-list (quotient n 10)))]
)
  )

(define (make-template xs)
  (cond
    [(null? xs) '()]
    [else (cons (cons (car xs) 0) (make-template (cdr xs)))]
    )
  )

(define (get-distribution n)
  (if (< n 0)
      (error "N must be positive")
      '())
  (define template (make-template (remove-duplicates (sort (make-list (* n n)) <))))
  
  (define (helper xss left-over)
    (cond
      [(= 0 left-over) xss]
      [else (helper (map (lambda (x) (remainder left-over 10)) xss) (quotient left-over 10))]
      ;smth should be modified in this section in order only to change the values in the template :)
  )
    )
  (helper template (* n n))
  )

(get-distribution 123); → '((1 . 2) (2 . 1) (5 . 1) (9 . 1))
;(get-distribution -123); → error