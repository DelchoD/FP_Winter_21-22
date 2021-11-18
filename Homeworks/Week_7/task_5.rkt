#lang racket

;Task_5
#|Define the predicate (triangular? Mat) it receives
 a square numeric matrix, represented as a list of lists,
 and checks whether it is upper triangular, i.e. whether
 all the elements are zeros below its main diagonal.|#

(define (triangular? matrixss)
  (if (not (= (sqrt (length (flatten matrixss))) (length matrixss))) (error "The maxtrix is not n X n") 0)
  (define (helper xss iter)
    (cond
      [(null? xss) #t]
      [(= (apply + (take (car xss) iter)) 0) (helper (cdr xss) (add1 iter))]
      [else #f]
      )
    )
  (helper (cdr matrixss) 1)
  )



(triangular? '((1 2 3)
               (0 5 6)
               (0 0 9))) ; -> #t
(triangular? '((0 2 3)
               (0 0 6)
               (1 0 0))) ; -> #f
(triangular? '((1 2 3)
               (1 5 6)
               (0 0 9))) ; -> #f
(triangular? '((1 2 3 4)
               (0 5 6 7)
               (0 0 8 9)
               (0 0 0 9))) ; -> #t
#|(triangular? '((1 2 3)
               (0 5 6 6)
               (0 0 9))) ; -> error "The maxtrix is not n X n"|#