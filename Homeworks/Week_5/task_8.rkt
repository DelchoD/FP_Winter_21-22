#lang racket

;Task_8
#|Take 2 strings s1 and s2 including only letters from a to z.
 Return a new sorted string, the longest possible, containing
distinct letters - each taken only once - coming from s1 or s2.|#

(define (longest xs ys)
  (list->string (sort (remove-duplicates (append (string->list xs) (string->list ys))) char<?)))

(equal? (longest "xyaabbbccccdefww" "xxxxyyyyabklmopq") "abcdefklmopqwxy")
(equal? (longest "abcdefghijklmnopqrstuvwxyz" "abcdefghijklmnopqrstuvwxyz") "abcdefghijklmnopqrstuvwxyz")



