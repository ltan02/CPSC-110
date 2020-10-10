;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname strictly-decreasing-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment accumulators-p3)
(@cwl ??? ???)

(@problem 1)
;; Design a function that consumes a list of numbers and produces true if the 
;; numbers in lon are strictly decreasing. You may assume that the list has at 
;; least two elements.

(@htdf strictly-decreasing)
(@signature (listof Number) -> Boolean)
;; true if the numbers in lon are strictly decreasing, otherwise false
;; ASSUME: list has at least two elements
(check-expect (strictly-decreasing (list 4 3 2 1 -2)) true)
(check-expect (strictly-decreasing (list 4 3 2 4 1 -2)) false)

;(define (strictly-decreasing lon) false) ;stub

(@template (listof Number) encapsulated accumulator)
(define (strictly-decreasing lon0)
  ;; acc is Number ; the previous number in the list
  (local [(define (solve lon acc)
            (cond [(empty? lon) true]
                  [else
                   (if (< (first lon) acc)
                       (solve (rest lon) (first lon))
                       false)]))]
    (solve (rest lon0) (first lon0))))
