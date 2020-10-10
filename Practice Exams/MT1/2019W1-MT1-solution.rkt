;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2019W1-MT1-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;(foo F1 (or F2 F3))
;(foo false (or F2 F3))
;(foo false (or true F3))
;(foo false true)
;(and false true)
;false


(@problem 3)
;; IGNORE THIS PROBLEM, IT WAS MUCH HARDER THAN WE INTENDED IT TO BE.
;;
;; The following function design may have errors and/or other design flaws.
;; Please fix the error(s) and/or flaw(s) that you find.  PLEASE NOTE THAT:
;;
;;   - Any changes you make should preserve the existing design intent.
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
#;
(define (largest x y z)
  (cond [(and (> x y) (> x z)) x]
        [(and (> y x) (> y z)) y]
        [(and (> z x) (> z y)) z]))

(check-expect (largest 3 3 1) 3)  ;add pairwise =
(check-expect (largest 3 1 3) 3)
(check-expect (largest 1 3 3) 3)

(check-expect (largest 1 2 1) 2)  ;add pairwise smaller
(check-expect (largest 2 1 1) 2)
(check-expect (largest 1 1 2) 2)


;; Adding only =.  Many other combinations would work;
;; in fact, could redundantly add = to every comparison.
#;
(define (largest x y z)
  (cond [(and (>= x y) (>= x z)) x]
        [(and (> y x) (>= y z)) y]
        [(and (> z x) (> z y)) z]))

#;
(define (largest x y z)
  (cond [(and (>= x y) (>= x z)) x]
        [(and (> y x) (>= y z)) y]
        [(and (> z x) (> z y)) z]))

;; Only deletions.
#;
(define (largest x y z)
  (cond [(and (> x y) (> x z)) x]
        [(> y z) y]
        [else z]))


;; Combinations of = and removing some redundant tests.
#;
(define (largest x y z)
  (cond [(and (>= x y) (> x z)) x]
        [(and (> y x) (> y z)) y]
        [else z]))


;; THESE ARE ALL INCORRECT
#;
(define (largest x y z)
  (cond [(and (> x y) (> x z)) x]
        [(and (> y x) (> y z)) y]
        [else z]))
#;
(define (largest x y z)
  (cond [(and (> x y) (> x z)) x]
        [(and (> y x) (> y z)) y]
        [(and (> z x) (> z y)) z]
        [(and (= x y) (= x z)) x]))


(define (largest x y z)
  (cond [(and (> x y) (> x z)) x]
        [(and (> y x) (> y z)) y]
        [(and (> z x) (> z y)) z]
        [(and (= x y) (> x z)) x]
        [(and (= x y) (= x z)) x]))


(@problem 4)
;;
;; Please write the template rules used tag and template for the following type
;; comment.  
;;
;; NO simplification is required, but the template must work correctly for all
;; data described by this

(define-struct apple (kind weight))
;; Foo is one of:
;;  - Number
;;  - "x"
;;  - "y"
;;  - (make-apple String Number)

(@dd-template-rules one-of
                    atomic-non-distinct
                    atomic-distinct
                    atomic-distinct
                    compound)

(define (fn-for-foo f)
  (cond [(number? f) (... f)]
        [(and (string? f) (string=? f "x")) (...)]
        [(and (string? f) (string=? f "y")) (...)] ;ok w/o guard
        [else
         (... (apple-kind f)
              (apple-weight f))]))



(@problem 5)
;;
;; Design a function that consumes an image, a width, and a height. The
;; function should determine whether the image fits inside of a rectangle
;; with the given width and height.  You must include all relevant recipe
;; elements and @tags.
;;
;; TO SAVE SPACE when writing your check-expects, you may assume that rect
;; is a function that consumes JUST width and height and produces a solid
;; black rectangle of that height.  So
;;  (rect 2 3) is the equivalent of (rectangle 2 3 "solid" "black")
;;
(@htdf fits?)
(@signature Image Natural Natural -> Boolean)  ;Number would have been OK
;; produce true if image w, h are < given w, h
(check-expect (fits? (rect 10 20) 11 21) #t)
(check-expect (fits? (rect 10 20) 11 20) #f)
(check-expect (fits? (rect 20 30) 20 31) #f)
(check-expect (fits? (rect 30 20) 31 19) #f)
(check-expect (fits? (rect 10 20)  9 21) #f)

;(define (fits? img w h) false) ;stub

(@template Image)                              ;the number type is OK

(define (fits? img w h)
  (and (< (image-width img) w)
       (< (image-height img) h)))


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
(define L3 (make-ln  7 "3456789"))
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
;;   ;; Natural -> String
;;   ;; produce a string with n spaces
;;
;; NOTE do not design padding.  Assume that padding exists and design
;; the justify function described above.  No marks will be given to solutions
;; that include a design for the padding function.
;;
(define FILL-TO 10)

(@htdf justify)
(@signature ListOfLine -> ListOfLine)
;; Right justify the lines to FILL-TO.
;; CONSTRAINT: No line will have more than FILL-TO characters.
(check-expect (justify empty) empty)
(check-expect (justify LOL3)
              (cons (make-ln 10 "0123456789")
                    (cons (make-ln 10 " 123456789")
                          (cons (make-ln 10 "        89")
                                empty))))

(@template ListOfLine)

(define (justify lol)
  (cond [(empty? lol) empty]
        [else
         (cons (justify-line (first lol))
               (justify  (rest lol)))]))

(@htdf justify-line)
(@signature Line -> Line)
;; right justify line to FILL-TO
;; CONSTRAINT: line will not have more than FILL-TO characters.
(check-expect (justify-line L0) L0)
(check-expect (justify-line L1) (make-ln 10 " 123456789"))
(check-expect (justify-line L8) (make-ln 10 "        89"))

(@template Line)

(define (justify-line l)
  (make-ln FILL-TO
           (string-append (padding (- FILL-TO (ln-l l)))
                          (ln-str l))))




;;
;; Helpers below here.
;;
(define (padding n)
  (cond [(zero? n) ""]
        [else
         (string-append " " (padding (sub1 n)))]))

(define (rect w h) (rectangle w h "solid" "black"))



