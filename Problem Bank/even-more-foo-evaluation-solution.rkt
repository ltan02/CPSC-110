;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname even-more-foo-evaluation-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment bsl-p7)
(@cwl ??? ???)

(@problem 1)
;;
;; Given the following function definition:
;;
(define (farfle s)
  (string-append s s))
;;
;; Write out the step-by-step evaluation of the expression: 
;;
;; (farfle (substring "abcdef" 0 2))
;;
;; Be sure to show every including the first evaluation step.


(farfle (substring "abcdef" 0 2))

(farfle "ab")

(string-append "ab" "ab")

"abab"