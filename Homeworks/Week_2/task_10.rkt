#lang racket

;Task_10
#|Define a predicate that accepts a natural number
 n and returns whether n2 ends in the digits of n.
|#

(define (count-digits-rec n)
  (cond
    [(zero? n) 0]
    [(not (positive? n)) (error "n was not positive")]
    [(< n 10) 1]
    [else (+ 1 (count-digits-rec (quotient n 10)))]
  )
  )

(define (automorphic? n)
  (cond
    [(<= n 0) (error "N is not natural number")]
    [(= n (remainder (* n n) (expt 10 (count-digits-rec n)))) #t]
    [else #f]
    )
  )
  



(equal? (automorphic? 3)#f)
(equal? (automorphic? 10)#f)
(equal? (automorphic? 5)#t)
(equal? (automorphic? 25)#t)
(equal? (automorphic? 76)#t) 
(equal? (automorphic? 890625)#t) 
(equal? (automorphic? 625)#t) 
(equal? (automorphic? 36) #f)
(equal? (automorphic? 11) #f)
;(automorphic? -1) ; error: n was not natural
;(automorphic? 0) ; error: n was not natural