#lang racket
(define (flat xs)
  (cond
    [(null? xs) (flatten xs)]
    [else (list* (flatten (car xs)) (flat (cdr xs)))]
    )
  )

(define (remove-doubles xs)
    (cond
      [(= (length xs) 1)  xs]
      [(equal? (car xs) (second xs)) (cons (car xs) (remove-doubles (cddr xs)))]
      [else (cons (car xs) (remove-doubles (cdr xs)))]
      )
  )

(define (itinerary xs)
 (λ (start)
 (define trips (sort (map remove-doubles (filter (λ (x) (equal? (car x) start)) (flat (permutations xs)))) (λ (x y) (< (length x) (length y)))))
   (car trips)
   #|(define (helper zs)
    (cond
      [(and (= 1 (length zs)) (equal? (caar zs) (last (car zs)))) (error "No such itinerary!")]
      [(equal? (remove-duplicates (car zs)) (sort (remove-duplicates (car zs)) string<?)) (car zs)]
      [else (helper (cdr zs))]
    )
  )|#
   #|The idea of the above functon is follow: If the length of the list with all trips
is 1 and there are two equal elememts in both ends we throw error because this is the only conbination
and it is obviously that is cannot happen. If not we start iterating the list of trips where the trips
are sorted by their length and we check if an element of the trips after it was sorted and any repetative 'airports' are deleted
equals with the the same element only with deletion of the repetative elements, if that is true we return the element. As for me this function should
work but somehow when I try to make a recursion in the lambda function it does not happen as i want|#
    ;(helper trips)
    
    )
  )

((itinerary '(("SFO" . "HKO") ("YYZ" . "SFO") ("YUL" . "YYZ") ("HKO" . "ORD"))) "YUL")
((itinerary '(("A" . "B") ("A" . "C") ("B" . "C") ("C" . "A")))"A")
((itinerary '(("SFO" . "COM") ("COM" . "YYZ"))) "COM")

;---------------------------------TASK 2--------------------------------------------------
; the main idea is to have function for making a list with n elements like el and
; one function which add el at the end and in the beggining

(define (generate-row el times)
  (define (helper iter xs)
    (if (= iter times)
       xs
      (cons el (helper (add1 iter) xs))
      )
    )
  (helper 0 '())
  )

(define (insert-at x position xs)
  (append (take xs (sub1 position)) (list x) (drop xs (sub1 position)))
)

(define (add-start-end el xs)
    (cond
    [(null? xs) xs]
    [else (cons (insert-at el (+ 1(length (flatten (insert-at el 1 (car xs))))) (insert-at el 1 (car xs))) (add-start-end el (cdr xs)))]
    )
    )

(define (pad xs)
  (λ (x)
     (list (generate-row x (+ (length xs) 2)) (add-start-end x xs) (generate-row x (+ (length xs) 2))) 
  )
  )

((pad '( (1 2 3)
 (4 5 6)
 (7 8 9) )
) "a")

((pad '( (1 2 3 4)
 (5 6 7 8)
 (9 10 11 12)
 (13 14 15 16))
) 9)


