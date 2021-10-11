#lang racket

;Task_5
#|A snail crawls up a column. During the day it crawls up some distance.
During the night it sleeps, so it slides down for some
 distance (less than it crawls up during the day).

Your procedure accepts three arguments:

The height of the column;
The distance that the snail crawls during the day;
The distance that the snail slides down during the night.
Calculate number of days the snail will need to reach the top of the column.
|#


#|Can be made with:
    1. Sigle function with proportion assumption
    2. Recursion
    3. The Provisioned For "loop" in lang Racket
|#

(define (snail columnHeight distanceDay distanceNight)
  (cond
    [(or(< columnHeight 0) (< distanceDay 0) (< distanceNight 0)) (error "Values can't be negative")]
    [else (max 1 (ceiling (/ (- columnHeight distanceNight ) (- distanceDay distanceNight))))]
  )
  )

(= (snail 3 2 1) 2)
(= (snail 10 3 1) 5)
(= (snail 10 3 2) 8)
(= (snail 100 20 5) 7)
(= (snail 5 10 3) 1)
