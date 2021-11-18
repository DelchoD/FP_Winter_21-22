#lang racket

;Task_1
#|Define a predicate that takes two non-empty lists
 of lists of numbers. If xss = '(xs1 .. xsn) and yss = '(ys1 .. ysn)
the procedure should return whether all pairs of elements (xsi, ysi) have equal lengths..|#

(define (have-matching-lengths xss yss)
  (cond
    [(not (= (length xss) (length yss))) #f]
    [(and (null? xss)(null? yss)) #t]
    [(not (= (length (car xss)) (length (car yss)))) #f]
    [else (have-matching-lengths (cdr xss) (cdr yss))]
    )
  )

(equal? (have-matching-lengths '((1 2 3) (4 5 6) (7 8 9)) '((1 4 7) (2 5 8) (3 6 9))) #t)
(equal? (have-matching-lengths '((1 2)) '((1 4 7) (2 5 8))) #f)
(equal? (have-matching-lengths '() '()) #t)
