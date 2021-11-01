#lang racket

;Task_2
#|Define a procedure repeater str that accepts a string
and returns a procedure of two arguments - count (number)
and glue (string). The result from a call to repeater should
be a string that is str repeated count times with glue being
put between every two str instances.
|#

(define (repeater str)
  (λ (count glue)
    (cond
     [(= count 1) str]
     [else (string-append str glue ((repeater str) (- count 1) glue))]
     )
  )
)

(equal? ((repeater "I love Racket") 3 " ") "I love Racket I love Racket I love Racket")
(equal? ((repeater "Quack") 5 "!") "Quack!Quack!Quack!Quack!Quack")
(equal? ((repeater " ") 10 " ") "                   ")

