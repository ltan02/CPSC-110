;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lec19-sum-tr-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
;;
;; PROBLEM:
;;
;; (A) Consider the following function that consumes a list of numbers and
;;     produces the sum of all the numbers in the list. Use the stepper to
;;     analyze the behavior of this function as the list gets larger and
;;     larger. 
;;    
;; (B) Use an accumulator to design a tail-recursive version of sum.

(@htdf sum)
(@signature (listof Number) -> Number)
;; produce sum of all elements of lon
(check-expect (sum empty) 0)
(check-expect (sum (list 2 4 5)) 11)

(@template (listof Number))

(define (sum lon)
  (cond [(empty? lon) 0]
        [else
         (+ (first lon)
            (sum (rest lon)))]))


(@htdf sum-tr)
(@signature (listof Number) -> Number)
;; produce sum of all elements of lon
(check-expect (sum-tr empty) 0)
(check-expect (sum-tr (list 2 4 5)) 11)

(@template (listof Number) accumulator)

(define (sum-tr lon0)
  ;; rsf is Number; sum of numbers before (first lon) in lon0
  ;;  (sum-tr (list 2 4 5))
  ;;  (sum-tr (list 2 4 5) 0)
  ;;  (sum-tr (list   4 5) 2)
  ;;  (sum-tr (list     5) 6)
  (local [(define (sum-w/acc lon rsf)
            (cond [(empty? lon) rsf]
                  [else                  
                   (sum-w/acc (rest lon) (+ rsf (first lon)))]))]

    (sum-w/acc lon0 0)))

