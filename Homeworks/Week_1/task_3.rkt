#lang racket

;Task_3
#|John has a backpack. With it he can carry k kilograms.
 An item from the supermarket weighs w kilograms.

 Define a predicate that accepts three numbers - c (number of products), k and w and returns
 whether John is capable of buying all the products in one trip to the supermarket.|#

(define (can-carry? number-of-products backpack-kilograms groceries-weight)
  (cond
    [(or (negative? number-of-products) (negative? backpack-kilograms)(negative? groceries-weight)) error]
    [(<=(* number-of-products groceries-weight) backpack-kilograms) #t]
    [else #f]
    )
  )





(equal? (can-carry? 5 15 3) #t)
(equal? (can-carry? 1 5 4) #t)
(equal? (can-carry? 13 25 2) #f)
(equal? (can-carry? 24 104.44 21.12) #f)
(equal? (can-carry? 51 34.75 19.852) #f)
(equal? (can-carry? 42 95.11 0.51) #t)

(can-carry? -13 25 2) ; error: The number of products was negative
(can-carry? 13 -25 2) ; error: John's hosting capacity was negative
(can-carry? 13 25 -2) ; error: The weight of a product was negative