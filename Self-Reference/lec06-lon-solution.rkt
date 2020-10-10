;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec06-lon-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@htdd ListOfNumber)
;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers
(define LON1 empty)
(define LON2 (cons 1 (cons 2 (cons 3 empty))))

(@dd-template-rules one-of             ;2 cases
                    atomic-distinct    ;empty
                    compound           ;(cons Number ListOfNumber)
                    self-ref)          ;(rest lon) is ListOfNumber

(define (fn-for-lon lon)
  (cond [(empty? lon) (...)]
        [else
         (... (first lon)
              (fn-for-lon (rest lon)))]))


#|
PROBLEMs:

Design a function that computes the sum of a list of numbers.

Design a function that counts the number of elements in a
list of numbers.

Design a function that produces a new list, where each element  
is 2 times the corresponding element in the original list.
|#

(@htdf sum)
(@signature ListOfNumber -> Number)
;; produce the sum of a list of numbers
(check-expect (sum empty) 0)
(check-expect (sum (cons 1 (cons 2 (cons 3 empty)))) (+ 1 2 3 0))

;(define (sum lon) 0)

(@template ListOfNumber)

(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))

(@htdf product)
(@signature ListOfNumber -> Number)
;; produce the product of a list of numbers
(check-expect (product empty) 1)
(check-expect (product (cons 3 (cons 4 (cons 5 empty)))) (* 3 4 5 1))

;(define (product lon) 0)

(@template ListOfNumber)

(define (product lon)
  (cond [(empty? lon) 1]
        [else
         (* (first lon)
            (product (rest lon)))]))

(@htdf count)
(@signature ListOfNumber -> Number)
;; produce the number of elements in a list of numbers
(check-expect (count empty) 0)
(check-expect (count (cons 3 (cons 4 (cons 5 empty)))) (+ 1 1 1 0))

;(define (count lon) 0)

(@template ListOfNumber)

(define (count lon)
  (cond [(empty? lon) 0]
        [else
         (+ 1
            (count (rest lon)))]))


(@htdf doubles)
(@signature ListOfNumber -> ListOfNumber)
;; produce list of 2 * every number in list
(check-expect (doubles empty) empty)
(check-expect (doubles (cons 1 (cons 2 (cons 3 empty))))
              (cons 2 (cons 4 (cons 6 empty))))

;(define (doubles lon) empty)

(@template ListOfNumber)

(define (doubles lon)
  (cond [(empty? lon) empty]
        [else
         (cons (* 2 (first lon))        
               (doubles (rest lon)))])) 