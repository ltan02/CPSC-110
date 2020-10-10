;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 2018W2MT1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require spd/tags)

(@problem 1)
;;
;; PUT YOUR CS ID HERE ON THIS LINE: ____________________________
;;


(@problem 2)
;;
;; Given the following definitions:
;;

(define RATE 3)
(define LIMIT 20)

(define (foo a b)
  (if (or (<= (+ a RATE) LIMIT)
          (> (* b 2) LIMIT))
      (+ a RATE)
      LIMIT))

;; Show the step by step evaluation of the following expression:

(foo (+ 10 3) (* 2 3))

























;; This page is blank in case you need it for Problem 2
















































(@problem 3)
;;
;; During the design of the following function, one step of the recipe was
;; completed incorrectly.  Please fix the design below to make it correct.
;; Remember our hint that only one step of the recipe was completed
;; incorrectly.
;;
;; You may assume that a correct standard ListOfNumber data definition exists
;; in this program.
;;

(@htdf contains-negative?)
(@signature ListOfNumber -> Boolean)
;; produce true if lon contains at least one negative number, false otherwise

(check-expect (contains-negative? empty) false)
(check-expect (contains-negative? (cons -5 empty)) true)
(check-expect (contains-negative? (cons 6 (cons 3 empty))) false)
(check-expect (contains-negative? (cons 6 (cons -3 empty))) true)

;(define (contains-negative? lon) false)   ;stub

(@template ListOfNumber)

(define (contains-negative? lon)
  (cond [(empty? lon) false]
        [else
         (if (negative? (first lon))
             true
             (fn-for-lon (rest lon)))])) 




















(@problem 4)
;;
;; During the design of the following function, one step of the recipe was
;; completed incorrectly.  Please fix the function design below to make it
;; correct. Remember our hint that only one step of the recipe was completed
;; incorrectly.
;;

(@htdd Blob)
(define-struct blob (w h vertical?))
;; Blob is (make-blob Natural Natural Boolean)
;; interp. a Blob with (w)idth (h)eight in pixels
;;         vertical? true is growing vertically, false is growing
;;         horizontally
;; < Examples, @dd-template-rules, and template deleted here >


(@htdf change-dir)
(@signature Blob KeyEvent -> Blob)
;; Blob grows horizontally after h pressed, vertically after v pressed
(check-expect (change-dir (make-blob 10 20 true) " ")
              (make-blob 10 20 true))
(check-expect (change-dir (make-blob 10 20 true) "h")
              (make-blob 10 20 false))
(check-expect (change-dir (make-blob 10 20 true) "v")
              (make-blob 10 20 true))
(check-expect (change-dir (make-blob 50 30 false) " ")
              (make-blob 50 30 false))
(check-expect (change-dir (make-blob 50 30 false) "h")
              (make-blob 50 30 false))
(check-expect (change-dir (make-blob 50 30 false) "v")
              (make-blob 50 30 true))
                           
;(define (change-dir b ke) b)     ;stub

(@template KeyEvent Blob)

(define (change-dir b ke)
  (cond [(key=? ke "h")
         (make-blob (blob-w b) (blob-h b) (blob-vertical? false))]  
        [(key=? ke "v")
         (make-blob (blob-w b) (blob-h b) (blob-vertical? true))]   
        [else b]))







(@problem 5)
;; 
;; Given the following comments, draw the reference arrows and label each
;; arrow with one of R or SR.  
;; 



;; Circle is Integer



;; Rectangle is (make-rectangle Natural Natural)



;; Shape is one of:
;; - Circle
;; - Rectangle



;; ListOfShape is one of:
;; - empty
;; - (cons Shape ListOfShape)

























(@problem 6)
;;
;; Given the following type comment and structure definition write the
;; @dd-template-rules tag and the template for Cooo.  You ONLY need
;; to write the dd-template-rules tag and template for Cooo. For full credit
;; you must do guarding and simplification if appropriate. A solution with
;; just correct guarding will receive more marks than a solution with
;; correct guarding and incorrect simplification.
;;
;; Note that a complete data definition for Baaa is provided on the next
;; page.
;;

(@htdd Cooo)
(define-struct huuu (intensity vis? col))
;; Cooo is one of:
;; - "cyborg"
;; - (make-huuu Number Boolean Baaa)
;; - Natural
































(@htdd Baaa)
;; Baaa is one of:
;; - "black"
;; - "red"
;; - "orange"
;; interp. the colors on planet X
;; <examples are redundant for enumerations>

(@dd-template-rules one-of            ; 3 cases
                    atomic-distinct   ; "black"
                    atomic-distinct   ; "red"
                    atomic-distinct)  ; "orange"

(define (fn-for-baaa b)
  (cond [(string=? b "black") (...)]
        [(string=? b "red") (...)]
        [(string=? b "orange") (...)]))

































(@problem 7)
;;
;; Consider the following data definitions.
;;

(@htdd Signal)
;; Signal is one of:
;; - Natural
;; - "walk"
;; - "wait"
;; interp. a traffic signal phase
;;          a number between 1 and 20 depicts the seconds remaining to cross,
;;         "walk" means pedestrians have time to cross the road,        
;;         "wait" means pedestrians should not cross until the next walk
;;         phase

(define S1 "walk")
(define S2 20)
(define S3 7)
(define S4 "wait")

(@dd-template-rules one-of               ; 3 cases
                    atomic-non-distinct  ; Natural
                    atomic-distinct      ; "walk"
                    atomic-distinct)     ; "wait"

(define (fn-for-signal s)
  (cond [(number? s) (... s)]
        [(string=? s "walk") (...)]
        [else (...)]))


(@htdd ListOfSignal)
;; ListOfSignal is one of:
;; - empty
;; - (cons Signal ListOfSignal)
;; interp. a list of traffic signals

(define LOS0 empty)
(define LOS1 (cons S1 (cons S2 (cons S3 (cons S4 empty)))))
 
(@dd-template-rules one-of           ; 2 cases
                    atomic-distinct  ; empty
                    compound         ; (cons Signal ListOfSignal)
                    ref              ; (first los) is Signal
                    self-ref)        ; (rest los) is ListOfSignal)





(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-signal (first los))
              (fn-for-los (rest los)))]))

;;
;; Able-bodied pedestrians are "able to cross" at a signal if the signal
;; either displays walk or a number greater than 8.
;;
;; Design a function that given a list of signals produces a count of the
;; number of signals that are crossable by able-bodied pedestrians.
;;
;; You must include all applicable @tags and follow all applicable design
;; rules.
;; 

































;; This page is blank in case you need it for Problem 7
















































