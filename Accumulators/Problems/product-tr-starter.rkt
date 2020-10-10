;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname product-tr-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment accumulators-p5)
(@cwl ??? ???)

(@problem 1)
;; (A) Consider the following function that consumes a list of numbers and
;;     produces the product of all the numbers in the list. Use the stepper 
;;     to analyze the behavior of this function as the list gets larger and
;;     larger.
;;    
;; (B) Use an accumulator to design a tail-recursive version of product.


(@htdf product)
(@signature (listof Number) -> Number)
;; produce product of all elements of lon
(check-expect (product empty) 1)
(check-expect (product (list 2 3 4)) 24)
#;
(@template (listof Number))
#;
(define (product lon)
  (cond [(empty? lon) 1]
        [else
         (* (first lon)
            (product (rest lon)))]))

(@template (listof Number) accumulator)
(define (product lon0)
  ;; rsf is Number ; the current product of elements seen so far
  (local [(define (solve lon rsf)
            (cond [(empty? lon) rsf]
                  [else
                   (solve (rest lon)
                          (* (first lon) rsf))]))]
    (solve lon0 1)))
