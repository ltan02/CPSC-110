;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lec18-sequencep-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;;

(@htdf sequence?)
(@signature (listof Natural) -> Boolean)
;; true if the list is a sequence (1 larger than the number before it)
;; ASSUME: list has at least one element
(check-expect (sequence? (list 2 3 4)) true)
(check-expect (sequence? (list 3 5 6)) false)

;(define (sequence? lon) false) ;stub

(@template (listof Number) accumulator)
(define (sequence? lon0)
  ;; acc: Natural ; the previous element in the list
  ;; (sequence? (list 2 3 4))
  ;;
  ;; (sequence? (list 3 4) 2)
  ;; (sequence? (list   4) 3)
  ;; (sequence? (list    ) 4)
  (local [(define (sequence? lon acc)
            (cond [(empty? lon) true]
                  [else
                   (if (= (add1 acc) (first lon))
                       (sequence? (rest lon) (first lon))
                       false)]))]
    (sequence? (rest lon0) (first lon0))))
