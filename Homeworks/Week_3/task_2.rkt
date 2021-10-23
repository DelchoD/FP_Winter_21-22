#lang racket

;Task_2
#|By using find-max and remove-first-occurrence,
define a procedure that sorts a number in descending order.
|#

(define (find-max n)
  (define (helper current-n max)
    (cond
      [(zero? current-n) max]
      [(> (remainder current-n 10) max) (helper (quotient current-n 10) (remainder current-n 10))]
      [else (helper (quotient current-n 10) max)]
      )
  )
  (helper (quotient n 10) (remainder n 10))
)

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

(define (sort-n number)
  (define (helper new-num left-over)
    (cond
      [(= left-over 0) new-num]
      [else (helper (+ (find-max left-over) (* new-num 10)) (remove-first-occurrence left-over (find-max left-over)))]
      )
    )
  (if (= (count-digits-rec (helper 0 number)) (count-digits-rec number))
  (helper 0 number)
  (* (helper 0 number) (expt 10 (- (count-digits-rec number) (count-digits-rec (helper 0 number)))))
  )
  )

(= (sort-n 1714) 7411)
(= (sort-n 123450) 543210)
(= (sort-n 123405) 543210)
(= (sort-n 123045) 543210)
(= (sort-n 120345) 543210)
(= (sort-n 102345) 543210)
(= (sort-n 8910) 9810)
(= (sort-n 321) 321)
(= (sort-n 29210) 92210)
(= (sort-n 1230) 3210)
(= (sort-n 55345) 55543)
(= (sort-n 14752) 75421)
(= (sort-n 329450) 954320)
(= (sort-n 9125) 9521)