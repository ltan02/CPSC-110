;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec06-los-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@htdd ListOfString)
;; ListOfString is one of:
;;  - empty
;;  - (cons String ListOfString)
;; interp. a list of strings
(define LOS1 empty)
(define LOS2 (cons "Canucks" empty))
(define LOS3 (cons "Leafs" (cons "Canucks" empty)))
(define LOS4 (cons "Canadiens" (cons "Leafs" (cons "Canucks" empty))))

(@dd-template-rules one-of             ;2 cases
                    atomic-distinct    ;empty
                    compound           ;(cons String ListOfString)
                    self-ref)          ;(rest los) is ListOfString

(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (first los)
              (fn-for-los (rest los)))])) 

#|
PROBLEM:

Design a function that determines whether "Canucks" appears in a 
list of strings.
|#

(@htdf contains-canucks?)
(@signature ListOfString -> Boolean)
;; produce true if los contains "Canucks"
(check-expect (contains-canucks? empty) false)
(check-expect (contains-canucks? (cons "Canucks" (cons "Flames" empty))) #t)
(check-expect (contains-canucks? (cons "Flames" (cons "Canucks" empty))) #t)
(check-expect (contains-canucks? (cons "Flames" (cons "Leafs" empty)))   #f)

;(define (contains-canucks? los) false)

(@template ListOfString)

(define (contains-canucks? los)
  (cond [(empty? los) false]
        [else
         (if (string=? (first los) "Canucks")
             true
             (contains-canucks? (rest los)))]))


