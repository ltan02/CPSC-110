;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2019W1-MT1-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)
(require 2htdp/image)


(@problem 1)
;;
;; PUT YOUR CS ID HERE ON THIS LINE: ____________________________
;;


(@problem 2)
;;
;; Given the following definitions:
;;

(define F1 false)
(define F2 true)
(define F3 false)

(define (foo a b)
  (and a b))

;; Show the step by step evaluation of the following expression:

(foo F1 (or F2 F3))























(@problem 3)
;; IGNORE THIS PROBLEM, IT WAS MUCH HARDER THAN WE INTENDED IT TO BE.
;;
;; The following function design may have errors and/or other design flaws.
;; Please fix the error(s) and/or flaw(s) that you find.  PLEASE NOTE THAT:
;;
;;   - You may not change the purpose at all, you may not add a CONSTRAINT.
;;   - Don't change things that do not need to change.  Do not entirely
;;     rewrite the function to fix a small problem or small problems.
;;
(@htdf largest)
(@signature Number Number Number -> Number)
;; produce the largest of three numbers
(check-expect (largest 3 2 1) 3)
(check-expect (largest 2 1 3) 3)
(check-expect (largest 2 3 1) 3)

(@template Number)

(define (largest x y z)
  (cond [(and (> x y) (> x z)) x]
        [(and (> y x) (> y z)) y]
        [(and (> z x) (> z y)) z]))


























(@problem 4)
;;
;; Please write the template rules used tag and template for the following type
;; comment.  
;;
;; The template must work correctly for all data described by this type comment.
;; You are not required to simplify the cond questions, but you are permitted
;; to do as long as the template still works correctly.

(define-struct apple (kind weight))
;; Foo is one of:
;;  - Number
;;  - "x"
;;  - "y"
;;  - (make-apple String Number)

































(@problem 5)
;;
;; Design a function that consumes an image, a width, and a height. The
;; function should determine whether the image fits inside of a rectangle
;; with the given width and height.  You must include all relevant recipe
;; elements and @tags. This includes a commented out stub.  You do NOT
;; need to have a commented out template.
;;
;; TO SAVE SPACE when writing your check-expects, you may assume that rect
;; is a primitive function that consumes JUST width and height and produces
;; a solid black rectangle of that height.  So
;;  (rect 2 3) is the equivalent of (rectangle 2 3 "solid" "black")
;; You can also abbreviate check-expect to c-e.
;;


































(@problem 6)
;;
;; Consider the following data definitions.
;;

(@htdd Line)
(define-struct ln (l str))
;; Line is (make-ln Natural String)
;; interp. a line of text, with it's length (number of characters)
;;         and a string representing the actual characters
;; CONSTRAINT: l must always have the same number of characters as the string

(define L0 (make-ln 10 "0123456789"))
(define L1 (make-ln  9 "123456789"))
(define L8 (make-ln  2 "89"))

(@dd-template-rules compound)

(define (fn-for-line ln)
  (... (ln-l ln) (ln-str ln)))



(@htdd ListOfLine)
;; ListOfLine is one of:
;;  - empty
;;  - (cons Line ListOfLine)
;; interp. a list of lines

(define LOL0 empty)
(define LOL1 (cons L0 empty))
(define LOL3 (cons L0 (cons L1 (cons L8 empty))))


(@dd-template-rules one-of atomic-distinct compound ref self-ref)

(define (fn-for-lol lol)
  (cond [(empty? lol) (...)]
        [else
         (... (fn-for-line (first lol))
              (fn-for-lol (rest lol)))]))
  






;;
;; One operation that a text editor can perform is to right-justify text.
;; This means adding enough spaces to the beginning of each line so that
;; the end of each line reaches a certain column.  Assuming the fill column
;; is 10, then (justify LOL3)  should produce:
;;
;;              (cons (make-ln 10 "0123456789")
;;                    (cons (make-ln 10 " 123456789")
;;                          (cons (make-ln 10 "        89")
;;                                empty))))
;;
;; Please design a justify function that consumes a list of lines and right
;; justifies those lines to FILL-TO.  You must follow all applicable design
;; rules and include all appropriate @tags.  Please use the Line and ListOfLine
;; example constants defined above in your check-expects.
;; You should include the following CONSTRAINT after the purpose:
;;
;; CONSTRAINT: No line may have more than FILL-TO characters.
;;
;; There is a primitive function called padding that you may call.
;;   (padding n) -> String
;;     n : Natural
;;   Returns a string with n spaces.
;;
;; NOTE do not design padding.  Assume that padding exists and design
;; the justify function described above.  No marks will be given for 
;; designs for padding.
;;


;; START YOUR WORK ON THE NEXT PAGE

















;; This page is blank for Problem 6


(define FILL-TO 10)












































;; This page is blank in case you need it.















































;; This page is blank in case you need it.
