;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname pset-02-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER.
;;
(require spd/tags)
(require 2htdp/image)

(@assignment pset-02);Do not edit or remove this tag
(@cwl ??? ???)       ;Replace ??? with your cwl,
;;                   ;second ??? is replaced with partner cwl if you have one


(@problem 1)
;;
;; The following function design may have errors in it.  Please fix the error
;; or errors that you find.  Any changes you make should preserve the existing
;; design intent.  
;;
;; First uncomment the entire function design, and then fix the error.
;; If you are unable to find and fix the error, leave it commented out.
;; Note that when fixing a bug in an existing program your goal should
;; be to have a light touch.  If there is a small simple way to fix
;; something then do it the small and simple way.
;;
;; Your solution must include @htdf, @signature, and @template tags.
;; 
(@htdf stack)
(@signature Image Image -> Image)
;; stack images, widest on bottom, first on bottom if same width
(check-expect (stack (rectangle 10 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red"))
              (above (rectangle 10 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red")))
(check-expect (stack (rectangle 20 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red"))
              (above (rectangle 20 9 "solid" "red")
                     (rectangle 20 5 "solid" "blue")))
(check-expect (stack (rectangle 20 5 "solid" "blue")
                     (rectangle 10 9 "solid" "red"))
              (above (rectangle 10 9 "solid" "red")
                     (rectangle 20 5 "solid" "blue")))

;(define (stack i1 i2) i1)   ;stub

(@template Image)

(define (stack i1 i2)
  (if (>= (image-width i1) (image-width i2))
      (above i2 i1)
      (above i1 i2)))





(@problem 2)
;;
;; The following data design may have errors in it. Please fix the error
;; or errors that you find.  Any changes you make should preserve the existing
;; design intent.
;;
;; Your solution must include @htdd tag, @dd-template-rules tag, and
;; a NOT commented fn-for-site template.
;; 


(@htdd Site)
;; Site is one of:
;; - "Vancouver"
;; - "Okanagan"
;; - "Robson"
;; - "CDM"
;; interp. a UBC site
;; <Examples are redundant for enumerations>

(@dd-template-rules one-of
                    atomic-distinct
                    atomic-distinct
                    atomic-distinct
                    atomic-distinct)

(define (fn-for-site s)
  (cond [(string=? s "Vancouver") (...)]
        [(string=? s "Okanagan") (...)]
        [(string=? s "Robson") (...)]
        [(string=? s "CDM") (...)]))





(@problem 3)
;; 
;; Consider the following data definition for Likert ratings.
;; 

(@htdd Rating)
;; Rating is one of:
;;  - "n/a"
;;  - Natural
;; interp. a Likert rating on a survey, either not applicable or rating
;;         1 is least happy, 5 is most happy, 3 is neither happy nor unhappy
;; CONSTRAINT: if a natural, is always in [1, 5]
(define R1 "n/a")  ;didn't vote
(define R2 5)      ;happiness with UBC!

(@dd-template-rules one-of
                    atomic-distinct
                    atomic-non-distinct)

(define (fn-for-rating r)
  (cond [(and (string? r) (string=? r "n/a")) (...)]
        [else
         (... r)]))
         

;;
;; Ali wants to work with ratings, but first needs a function that
;; consumes a rating and produces true if it represents a clear
;; preference.  n/a and 3 not real preferences, 1/2/4/5 are.
;; Design this function, called it preference?
;;

(@htdf preference?)
(@signature Rating -> Boolean)
;; produce true if a real positive/negative preference
(check-expect (preference? "n/a") false)
(check-expect (preference? 1) true)
(check-expect (preference? 2) true)
(check-expect (preference? 3) false)
(check-expect (preference? 4) true)
(check-expect (preference? 5) true)

;(define (preference? r) false)  ;stub

(@template Rating)

(define (preference? r)
  (cond [(and (string? r) (string=? r "n/a")) false]
        [else
         (not (= r 3))]))


(@problem 4)
;;
;; Write the dd-template-rules tag and template for the following
;; type comment.  Your rules and template must be in the same order
;; as the type comment.
;;

(@htdd Altitude)
;; Altitude is one of:
;;  - "pre-launch"
;;  - Number
;;  - "post-flight"
;; interp. Altitude of rocket. Before launch, in meters above launch
;;         pad, after flight has ended.
;; CONSTRAINT: when a number is > 0
(define A0 "pre-launch")
(define A1 37.5)
(define A2 "post-flight")

(@dd-template-rules one-of
                    atomic-distinct
                    atomic-non-distinct
                    atomic-distinct)

(define (fn-for-altitude a)
  (cond [(and (string? a) (string=? a "pre-launch")) (...)]
        [(number? a) (... a)]
        [else (...)]))


(@problem 5)
;;
;; Design a function that consumes an Altitude and produces true
;; if the rocket is actually inflight.  Call the function inflight?
;;

(@htdf inflight?)
(@signature Altitude -> Boolean)
;; produce true if rocket is actually inflight; false otherwise
(check-expect (inflight? "pre-launch")  false)
(check-expect (inflight? "post-flight") false)
(check-expect (inflight? 23) true)

;(define (inflight? a) false) ;stub

(@template Altitude)

(define (inflight? a)
  (cond [(and (string? a) (string=? a "pre-launch")) false]
        [(number? a) true]
        [else false]))
  
