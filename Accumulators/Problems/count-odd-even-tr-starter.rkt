;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname count-odd-even-tr-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment accumulators-p9)
(@cwl ??? ???)

(@problem 1)
;; Previously we have written functions to count the number of elements in a
;; list. In this problem we want a function that produces separate counts of the
;; number of odd and even numbers in a list, and we only want to traverse the
;; list once to produce that result.
;;
;; Design a tail recursive function that produces the Counts for a given list of
;; numbers. Your function should produce Counts, as defined by the data
;; definition below.
;;
;; There are two ways to code this function, one with 2 accumulators and one
;; with a single accumulator. You should provide both solutions.


(@htdd Counts)
(define-struct counts (odds evens))
;; Counts is (make-counts Natural Natural)
;; interp. describes the number of even and odd numbers in a list

(define C1 (make-counts 0 0)) ;describes an empty list
(define C2 (make-counts 3 2)) ;describes (list 1 2 3 4 5))

#;
(define (fn-for-counts c)
  (... (counts-odds c)
       (counts-evens c)))

(@htdf count-odd-even)
(@signature (listof Number) -> Counts)
;; produce seperate counts of the number of odd and even numbers
(check-expect (count-odd-even empty) C1)
(check-expect (count-odd-even (list 1 2 3 4 5))
              (make-counts 3 2))

;(define (count-odd-even lon) C1) ;stub

#;#;
(@template (listof Number) encapsulated accumulator)
(define (count-odd-even lon0)
  ;; odd is Natural ; the number of odd numbers seen so far
  ;; even is Natural ; the number of even numbers seen so far
  (local [(define (solve lon odd even)
            (cond [(empty? lon) (make-counts odd even)]
                  [else
                   (if (odd? (first lon))
                       (solve (rest lon) (add1 odd) even)
                       (solve (rest lon) odd (add1 even)))]))]
    (solve lon0 0 0)))

(@template (listof Number) encapsulated accumulator)
(define (count-odd-even lon0)
  ;; acc is Counts ; the number of odds and evens seen so far
  (local [(define (solve lon acc)
            (cond [(empty? lon) acc]
                  [else
                   (solve (rest lon)
                          (if (odd? (first lon))
                              (make-counts (add1 (counts-odds acc))
                                           (counts-evens acc))
                              (make-counts (counts-odds acc)
                                           (add1 (counts-evens acc)))))]))]
    (solve lon0 (make-counts 0 0))))











