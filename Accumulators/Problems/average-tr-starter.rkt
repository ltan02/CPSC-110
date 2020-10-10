;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname average-tr-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment accumulators-p4)
(@cwl ??? ???)

(@problem 1)
;; Design a function called average that consumes (listof Number) and produces
;; the average of the numbers in the list. The function must be TAIL RECURSIVE.

(@htdf average)
(@signature (listof Number) -> Number)
;; produces the average of the numbers in the list
(check-expect (average (list 1 2 3)) 2)
(check-expect (average (list 4 10 29 3 -1 -10))
              (/ (+ 4 10 29 3 -1 -10) 6))

;(define (average lon) 0) ;stub

(define (average lon0)
  ;; acc is Natural ; the number of elements seen so far
  ;; rsf is Number ; the sum of the elements seen so far
  (local [(define (solve lon acc rsf)
            (cond [(empty? lon) (/ rsf acc)]
                  [else
                   (solve (rest lon) (add1 acc) (+ (first lon) rsf))]))]
    (solve lon0 0 0)))

