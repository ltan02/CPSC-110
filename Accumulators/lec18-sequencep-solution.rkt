;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lec19-sequencep-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
;;
;; PROBLEM:
;;
;; Design a function called sequence? that consumes a list of natural numbers
;; and produces true if the list is a sequence, meaning each number is 1 larger
;; than the number before it.  You may assume the list has at least one element.
;; So:
;;    (sequence? (list 2 3 4))  ==> true
;;    (sequence? (list 3 5 6))  ==> false

(@htdf sequence?)

(@signature (listof Natural) -> Boolean)
;; produces true if the list is a strict sequence
;; Assume: the list has at least one element.
(check-expect (sequence? (list 2))     true)
(check-expect (sequence? (list 2 3 4)) true)
(check-expect (sequence? (list 3 5 6)) false)

(@template (listof Number) accumulator)

(define (sequence? lon0)
  ;; acc is  Natural
  ;; invariant: the element of lon0 immediately before (first lon)
  ;; (sequence? (list 2 3 4 5 6))
  
  ;; (sequence? (list   3 4 5 6) 2)  
  ;; (sequence? (list     4 5 6) 3)
  ;; (sequence? (list       5 6) 4)
  (local [(define (sequence? lon acc)
            (cond [(empty? lon) true]
                  [else
                   (if (= (first lon) (+ 1 acc)) ;exploit
                       (sequence? (rest lon)
                                  (first lon))   ;preserve
                       false)]))]
    
    (sequence? (rest lon0)
               (first lon0))))                   ;initialize

