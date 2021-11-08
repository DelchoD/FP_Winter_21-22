#lang racket

;Task_1
#|Define a procedure that removes an element from a list.|#


(define (remove-all-no-proc elementToRemove xs)
  (flatten (cond
    [(null? xs) (flatten xs)] 
    [(equal? (car xs) elementToRemove) (cons '() (remove-all-no-proc elementToRemove (cdr xs)))]
    [else (cons (car xs) (remove-all-no-proc elementToRemove (cdr xs)))]
    ))
  )

(define (remove-all-proc elementToRemove xs)
  (remq* (flatten (cons elementToRemove '())) xs)
  )

; without using a predefined procedure
(equal? (remove-all-no-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-no-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-no-proc 1 '(1)) '())
(equal? (remove-all-no-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-no-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))

; using a predefined procedure
(equal? (remove-all-proc 1 '(1 1 1 2)) '(2))
(equal? (remove-all-proc 1 '(2 5 6)) '(2 5 6))
(equal? (remove-all-proc 1 '(1)) '())
(equal? (remove-all-proc 1 '(1 2 1 1)) '(2))
(equal? (remove-all-proc "RNN" '("CNN" "RNN" "GAN" "RNN")) '("CNN" "GAN"))