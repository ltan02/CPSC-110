;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab-08-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
i di;; CPSC 110 - Abstraction Lab

(require spd/tags)
(require racket/file)

(@assignment lab-08)
(@cwl atan12)

(@problem 1)
;; The first four problems use the following data definition,
;; which represents a path through a binary search tree.

(@htdd Path)
;; Path is one of:
;; - empty
;; - (cons "L" Path)
;; - (cons "R" Path)
;; interp. 
;;  A sequence of left and right 'turns' down through a binary tree
;;  (list "L" "R" "R") means take the left child of the root, then
;;  the right child of that node, and the right child again.
;;  empty means you have arrived at the destination.
(define P1 empty)
(define P2 (list "L" "R"))
(define P3 empty)
(define P4 (cons "L" (cons "R" empty)))
(define P5 (cons "L" (cons "R" (cons "R" empty))))

(@dd-template-rules one-of atomic-distinct self-ref self-ref)
(define (fn-for-path p)
  (cond [(empty? p) (...)]
        [(string=? (first p) "L") (... (fn-for-path (rest p)))]
        [(string=? (first p) "R") (... (fn-for-path (rest p)))]))




;;
;; Design an abstract function (including signature, purpose, and tests)
;; called num-lr to simplify the lefts-minus-rights and rights-minus-lefts
;; functions defined below.
;;
;; Then re-define the original lefts-minus-rights and rights-minus-lefts
;; functions to use your abstract function. Remember, the signature and tests
;; should not change from the original functions. For simplicity, assume 
;; that all numbers throughout this problem have type Integer.


(@htdf lefts-minus-rights)
(@signature Path -> Integer)
;; produce the difference between left turns and right turns

(check-expect (lefts-minus-rights empty) 0)
(check-expect (lefts-minus-rights (list "R" "L" "R")) -1)
(check-expect (lefts-minus-rights (list "L" "R" "L")) 1)

(@template Path)
(define (lefts-minus-rights p)
  (num-lr add1 sub1 p))


(@htdf rights-minus-lefts)
(@signature Path -> Integer)
;; produce the difference between right turns and left turns

(check-expect (rights-minus-lefts empty) 0)
(check-expect (rights-minus-lefts (list "R" "L" "R")) 1)
(check-expect (rights-minus-lefts (list "L" "R" "L")) -1)

(@template Path)
(define (rights-minus-lefts p)
  (num-lr sub1 add1 p))




;; Use the space below to design the abstract function for Problem 1:

(@htdf num-lr)
(@signature (Integer -> Integer) (Integer -> Integer) Path -> Integer)
;; produce the difference between turns
(check-expect (num-lr add1 sub1 empty) 0)
             
(define (num-lr c1 c2 p)
  (cond [(empty? p) 0]
        [(string=? (first p) "L") (c1 (num-lr (rest p)))]
        [(string=? (first p) "R") (c2 (num-lr (rest p)))]))

(@problem 2)
;;
;; Use your abstract function from the previous problem to design a function
;; called path-length that determines the length of a given path.

(@htdf path-length)
(@signature Path -> Integer)
;; produces the length of a given path
(check-expect (path-length empty) 0)
(check-expect (path-length (list "R" "L" "R" "L" "L")) 5)

; (define (path-length p) 0) ;stub

(@template use-abstract-fn)
(define (path-length p)
  (num-lr add1 add1 p))

(@problem 3)
;;
;; Design an abstract fold function for Path called fold-path

(@htdf fold-path)
(@signature X (X -> X) (X -> X) Path -> X)
;; an abstract fold function for Path
(check-expect (local [(define (c1 lpath)
                        (cons "L" lpath))
                      (define (c2 rpath)
                        (cons "R" rpath))]
                (fold-path empty c1 c2 (list "R" "L" "R")))
              (list "R" "L" "R"))
(check-expect (fold-path add1 add1 0 (list "R" "L" "R"))
              3)

(@template Path)
(define (fold-path b1 c1 c2 p)
  (cond [(empty? p) b1]
        [(string=? (first p) "L") (c1 (fn-for-path (rest p)))]
        [(string=? (first p) "R") (c2 (fn-for-path (rest p)))]))

(@problem 4)
;;
;; Use your fold-path function called path-string to design a function called
;; path-string that produces a single string that concatenates all of the turns
;; in a path.

(@htdf path-string)
(@signature Path -> String)
;; produces a single string that concatenates all of the turns in a path
(check-expect (path-string empty) "")
(check-expect (path-string (list "R" "L" "R")) "RLR")

;(define (path-string p) "") ;stub

(@template use-abstract-fn)
(define (path-string p)
  (local [(define (c1 lpath)
            (string-append "L" lpath))
          (define (c2 rpath)
            (string-append "R" rpath))]
    (fold-path "" c1 c2 p)))

(@problem 5)
;;
;; Design a function called popular-spring-class-count that takes a list of
;; class data and produces the number of classes from Term 2 where enrollment
;; exceeded 70% capacity (that is, enrollment / capacity > 0.7).
;;
;; The function that you design must make at least one call to 
;; built-in abstract functions.

(@htdd Class)
(define-struct class (id sec term credits enrolled capacity title))
;; Class is (make-class String String Natural
;;             Natural Natural Natural String)
;; interp. (make-class id sec term credits enrolled capacity title) is
;; data about a UBC CS class where:
;; - id is the class identifier
;; - sec is the class section
;; - term is the term during which the class is held, restricted to [1,2]
;; - credits is the number of credits the course is worth
;; - enrolled is the number of students enrolled
;; - capacity is the number of students that could be enrolled
;; - title is an abbreviated title for the course
;; CONSTRAINT: a class's capacity is always >= 1

(define C0 (make-class "CPSC229" "202" 2 4 40 84 "CMPTNL BSKT WVNG"))
(define C1 (make-class "CPSC259" "201" 2 4 190 188 "DTA&ALG ELEC ENG"))
(define C2 (make-class "CPSC400" "123" 1 0 190 188 "SNRITIS FOR MJRS"))

(@dd-template-rules compound)
(define (fn-for-class c)
  (... (class-id c)
       (class-sec c)
       (class-term c)
       (class-credits c)
       (class-enrolled c)
       (class-capacity c)
       (class-title c)))


;; If you would like to use the real daily class data from
;; UBC Pair (http://pair.ubc.ca), 
;; place the pair.ss file in your current directory and uncomment
;; the following definition and add the given check-expect to your
;; examples.

#;
(define COURSE-DATA 
  (local [(define (data->class d)
            (make-class (first d) (second d) (third d) (fourth d)
                        (fifth d) (sixth d) (seventh d)))]
    (map data->class (file->value "pair.ss"))))


;; Use the space below to design the function for Problem 5:

(@htdf popular-spring-class-count)

#;
(check-expect (popular-spring-class-count COURSE-DATA) 28)
















