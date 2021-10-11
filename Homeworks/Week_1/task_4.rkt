#lang racket

;Task_4
#|Each day a plant is growing by upSpeed meters. Each night that plants height decreases by downSpeed
 meters due to the lack of sun light. Initially, plant is 0 meters tall. We plant the seed at the
 beginning of a day. We want to know how many days it will take for the plant to reach a certain height.
|#

#| ! Take notice that we want to know how many DAYS so there is possibility that if the plant reaches the
 desired height beforenight it is also a day(not only 24h)
|#
#|Can be made with:
    1. Sigle function with proportion assumption
    2. Recursion
    3. The Provisioned For "loop" in lang Racket
|#


(define (growing-plant upSpeed downSpeed desiredHeight)
  (cond
    [(or(< desiredHeight 0)(< upSpeed 0)(< downSpeed 0))(error "Values can't be negative")]
    [else (max 1(ceiling (/ (- desiredHeight downSpeed) (- upSpeed downSpeed))))]
  ))


(= (growing-plant 5 2 5) 1)
(= (growing-plant 5 2 6) 2)
(= (growing-plant 10 9 4) 1)
(= (growing-plant 100 10 910) 10) ; upSpeed=100, downSpeed=10, desiredHeight=910