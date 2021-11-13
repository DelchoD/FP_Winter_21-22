#lang racket

(define (count-occurences number digit)
  (define (helper currentNum total)
  (cond
    [(and (< currentNum 10) (= currentNum digit)) (add1 total)]
    [(and (< currentNum 10) (not(= currentNum digit))) total]
    [(= (remainder currentNum 10) digit) (helper (quotient currentNum 10) (add1 total))]
    [else (helper (quotient currentNum 10) total)]
  )
  )
  (cond
    [(or (<= number 0) (<= digit 0)) (error "Values can not be negative")]
    [else (helper number 0)]
  )
  )

(define (sum-digits-iter n)
  (define (helper sum currentNum)
    (if (< currentNum 10)
        (+ sum currentNum)
        (helper (+ sum (remainder currentNum 10)) (quotient currentNum 10))
        )
    )
  (cond
    [(zero? n) 0]
    [(negative? n) (error "n was not positive")]
    [else (helper 0 n)]
  )
  )

(define (sum-counts-iter x d)
  (define (helper current-num sum)
    (cond
      [(< x 1) (error "Value can not be negative")]
      [(> current-num x) sum]
      [else (helper (add1 current-num) (+ sum (count-occurences current-num d)))]
      )
    )
  (sum-digits-iter (helper 1 0))
  )


(= (sum-counts-iter 1 1) 1) ; -> 1
(= (sum-counts-iter 5123 1) 19) ; -> 19
(= (sum-counts-iter 1234 8) 10) ; -> 10
(= (sum-counts-iter 5555 5) 10) ; -> 10
(= (sum-counts-iter 65432 6) 11) ; -> 11
(= (sum-counts-iter 70000 1) 11) ; -> 11
(= (sum-counts-iter 123321 1) 29) ; -> 29

;---------------------------TASK-2-----------------------------


(define (add-ones n)
  (define (helper new-num left-over iter flag)
    (cond
      [(zero? left-over) new-num]
      [(and (= (remainder left-over 10) 9) (= 0 iter)) (helper (+ (* (expt 10 (+ 1 iter)) ) new-num) (quotient left-over 10) (+ 1 iter) #t)]
      [(= (remainder left-over 10) 9) (helper (+ (* (expt 100 iter) ) new-num) (quotient left-over 10) (+ 1 iter) #t)]
      [(and (< (remainder left-over 10) 9) flag) (helper (+ (* (add1 (remainder left-over 10)) (expt 10 (+ 1 iter))) new-num) (quotient left-over 10) (add1 iter) #f)]
      [else (helper (+ (* (add1 (remainder left-over 10)) (expt 10 iter)) new-num) (quotient left-over 10) (add1 iter) #f)]
      )
    )
  (helper 0 n 0 #f)
  )
    
(= (add-ones 123) 234) ; -> 234
(= (add-ones 193) 2104) ; -> 2104
(= (add-ones 998) 10109) ; -> 10109
(= (add-ones 9999) 1010110) ; -> 1010110



















    


         