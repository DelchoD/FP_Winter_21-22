#lang racket

;Task_1
#|Define a procedure that removes the first occurrence
 of a digit in a number by going from right to left.
|#

(require math/number-theory)

(define (find-index number digit)
  (define (helper current-num index)
  (cond
    [(= (remainder current-num 10) digit) index]
    [else (helper (quotient current-num 10) (add1 index))]
    )
    )
  (helper number 0)
  )

(define (rev n)
  (define (helper result left-over)
    (if (zero? left-over)
        result
        (helper (+ (* result 10) (remainder left-over 10)) (quotient left-over 10))
        )
    )
  (helper 0 n)
  )

(define (count-digits-rec n)
  (cond
    [(zero? n) 0]
    [(not (positive? n)) (error "n was not positive")]
    [(< n 10) 1]
    [else (+ 1 (count-digits-rec (quotient n 10)))]
  )
  )

(define (remove-first-occurrence number digit)
  (define position (find-index number digit))
  (define (helper current-number index new-number)
    (cond
      [(= (count-digits-rec number) index) (+ (remainder current-number 10) (* new-number 10))]
      [(= index position) (helper (quotient current-number 10) (add1 index) new-number)]
      [else (helper (quotient current-number 10) (add1 index) (+ (remainder current-number 10) (* new-number 10)))]
)
    )
  
  (if (< (count-digits-rec (rev (helper number 0 0))) (sub1 (count-digits-rec number)))
      (* (rev (helper number 0 0)) (expt 10 (- (sub1 (count-digits-rec number)) (count-digits-rec (rev (helper number 0 0))))))
      (rev (helper number 0 0))
      )
  )


(= (remove-first-occurrence 15365 5) 1536)
(= (remove-first-occurrence 15360 0) 1536)
(= (remove-first-occurrence 15300 0) 1530)
(= (remove-first-occurrence 15365 1) 5365)
(= (remove-first-occurrence 35365 3) 3565)
(= (remove-first-occurrence 1212 1) 122)
(= (remove-first-occurrence 1212 2) 121)
(= (remove-first-occurrence (remove-first-occurrence 1212 1) 1) 22)