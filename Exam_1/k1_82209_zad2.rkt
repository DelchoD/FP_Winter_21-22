#lang racket

(define (get-list-of-all-lengts xss)
  (cond
    [(null? xss) '()]
    [else (sort (cons (length (car xss)) (get-list-of-all-lengts (cdr xss))) <)]
    )
  )

(define  (get-missing-length xss)
  (if (null? xss)
      (error "The list is empty!")
      '()
      )
  (define (helper xs)
    (cond
     ; [(null? xs) (error "There is no missing element")]
      [(= 0 (car xs)) (error "Single element can not be zero")]
      [(= (- (cadr xs)(car xs)) 2) (+ 1 (car xs))]
      [else (helper (cdr xs))]
      )
    )
  (helper (get-list-of-all-lengts xss))
  
  )
(get-missing-length '((1 2) (4 5 1 1) (1) (5 6 7 8 9))) ;→ 3
(get-missing-length '(("a", "a", "a") ("a", "a") ("a", "a", "a","a") ("a") ("a", "a", "a", "a", "a", "a"))); → 5
;(get-missing-length '((1 2) (4 5 1 1) () (1 2 3) (5 6 7 8 9))) ;→ error
;(get-missing-length '()) ;→ error
;(get-missing-length '((1))) ;→ error