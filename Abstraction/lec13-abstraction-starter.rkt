;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lec13-abstraction-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@htdd ListOfNumber)
;; ListOfNumber is one of:
;;  - empty
;;  - (cons Number ListOfNumber)
;; interp. a list of numbers


(@problem 1)
#|
Develop a function definition for an abstract function to simplify the 
following two functions. Complete your work by re-defining the original
functions to use the new abstract function. You do not need to do the
signature, purpose or tests for the new function.
|#

(@htdf all-correct?)
(define (all-correct? fn lon)
  (cond [(empty? lon) true]
        [else
         (and (fn (first lon))
              (all-correct? fn (rest lon)))]))

(@htdf all-greater?)
(@signature ListOfNumber Number -> Boolean)
;; produce true if every number in lon is greater than x.
(check-expect (all-greater? empty 0) true)
(check-expect (all-greater? (list 2 -3 -4) -6) true)
(check-expect (all-greater? (list -2 -3 -4) -3) false)

(@template use-abstract-fn)
(define (all-greater? lon x)
  (local [(define (greater? n)
            (> n x))]
    (all-correct? greater? lon)))

(@htdf all-positive?)
(@signature ListOfNumber -> Boolean)
;; produce true if every number in lon is positive?
(check-expect (all-positive? empty) true)
(check-expect (all-positive? (list 2 3 -4)) false)
(check-expect (all-positive? (list 2 3  4)) true)

(@template use-abstract-fn)
(define (all-positive? lon)
  (all-correct? positive? lon))

(@problem 2)
#|
Complete the design of the filter2 abstract function with signature,
purpose and tests.
|#

(@htdf positive-only)
(@signature ListOfNumber -> ListOfNumber)
;; produce list with only postive? elements of lon
(check-expect (positive-only empty) empty)
(check-expect (positive-only (list 1 -2 3 -4)) (list 1 3))

;(define (positive-only lon) empty)   ;stub

(@template use-abstract-fn)

(define (positive-only lon)
  (filter2 positive? lon))


(@htdf negative-only)
(@signature ListOfNumber -> ListOfNumber)
;; produce list with only negative? elements of lon
(check-expect (negative-only empty) empty)
(check-expect (negative-only (list 1 -2 3 -4)) (list -2 -4))

;(define (negative-only lon) empty)   ;stub

(define (negative-only lon)
  (filter2 negative? lon))




(@htdf filter2)
(@siganture (X -> Boolean) (listof X) -> X)
;; produce list of elements that satisfies p
(check-expect (filter2 false? (list true true false)) (list false))
(check-expect (filter2 odd? (list 1 2 3)) (list 1 3))
(check-expect (filter2 string? (list "" 2 "1")) (list "" "1"))

(@template (listof X))
(define (filter2 p lon)
  (cond [(empty? lon) empty]
        [else 
         (if (p (first lon))
             (cons (first lon) 
                   (filter2 p (rest lon)))
             (filter2 p (rest lon)))]))



(@problem 3)
;; 
;; Write the definition of all-greater-than using filter2. 
;;

(@htdf all-greater-than)
(@signature Number (listof Number) -> (listof Number))
;; produce list of all elements of lon > than n
(check-expect (all-greater-than 3 empty) empty)
(check-expect (all-greater-than 3 (list 1 4 2 5)) (list 4 5))

(define (all-greater-than n lon) empty)


(@problem 4)
;;
;; Complete the design of the map2 abstract function with signature.
;;

(@htdf squares)
(@signature (listof Number) -> (listof Number))
;; produce list of sqr of every number in lon
(check-expect (squares empty) empty)
(check-expect (squares (list 3 4)) (list 9 16))

(define (squares lon) (map2 sqr lon))

(@htdf square-roots)
(@signature (listof Number) ->(listof Number))
;; produce list of sqrt of every number in lon
(check-expect (square-roots empty) empty)
(check-expect (square-roots (list 9 16)) (list 3 4))

(define (square-roots lon) (map2 sqrt lon))


(@htdf map2)
;; given fn and (list n0 n1 ...) produce (list (fn n0) (fn n1) ...)
(check-expect (map2 sqr empty) empty)
(check-expect (map2 sqr (list 2 4)) (list 4 16))
(check-expect (map2 sqrt (list 16 9)) (list 4 3))
(check-expect (map2 abs (list 2 -3 4)) (list 2 3 4))

(define (map2 fn lon)
  (cond [(empty? lon) empty]
        [else
         (cons (fn (first lon))
               (map2 fn (rest lon)))]))

(@problem 5)
;;
;; Design an abstract function called foldr2 based on the (listof X) template.
;; Work backwards through the HtDF recipe starting from the fn definition.
;;












(define (fn-for-lox lox)
  (cond [(empty? lox) (...)]
        [else
         (... (first lox)
              (fn-for-lox (rest lox)))]))







