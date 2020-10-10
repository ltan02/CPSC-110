;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pset-07-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment pset-07); Do not edit or remove this tag
(@cwl atan12 ???)       ; Replace ??? with your cwl,
;;                   ; second ??? is replaced with partner cwl if you have one

(@problem 1)

(@htdd Letters)
;; Letters is one of:
;; - empty
;; - (cons String Letters)
;; interp. a sequence of letters
;; CONSTRAINT: every string in the sequence is only one letter

(define L1 empty)
(define L2 (list "a" "B" "c"))

(@htdd Operations)
;; Operations is one of:
;; - empty
;; - (cons "keep" Operations)
;; - (cons "space" Operations)
;; - (cons "remove" Operations)
;; interp. a sequence of operations to apply to a sequence of letters
;;         "keep"   means keep    the corresponding letter
;;         "space"  means replace the corresponding letter with a space   
;;         "remove" means remove  the corresponding letter

;;
;; In this problem you will design a function called decoder, that will
;; decode encrypted messages! Very poorly encrypted messages.  The function
;; consumes a Letters (l) and Operations (ops) in that order. The function
;; should produce a String which is the result of applying the given
;; operations to the successive and joining the resulting letters
;; together.
;;
;; For example:
;; (decoder (list "i" "k" "l" "h" "a" "m" "k" "s" "a" "r" "m")
;;          (list "keep" "remove" "remove" "space" "keep" "keep"
;;                "space" "keep" "keep" "remove"))
;;
;; should produce "i am sam"
;;
;; Notice that some of the letters were kept, some were replaced with a space,
;; and some were removed.
;; 
;; If ops runs out of operations before the end of the letters, then all 
;; the remaining letters in l are kept.
;;
;; If l runs out of letters first, then the remaining operations in ops are
;; ignored.
;;
;; Example 2:
;; (decoder (list "f" "a" "d" "e")
;;          (list "remove" "keep" "space" "keep" "remove" "keep" "space"))
;;
;; should produce "a e"
;;
;; You must solve this as a 2-one-of problem and your solution must include
;; a table. You should show the correspondence between table cells and
;; cond QA pairs. By this, we mean you should NUMBER the table cells and NUMBER
;; the corresponding cond QA pairs with the same numbers (as was done in
;; lecture). If you can simplify the table you should.  Finally, it is 
;; IMPERATIVE that your cond questions only include the kinds of questions that 
;; the data driven template rules produce, other conditional behaviour must go
;; into the cond answers. (This is the rule about not editing template questions
;; carried over into 2-one-of problems.)
;;

(@htdf decoder) ;!!! uncomment this when you start your function design
(@signature Letters Operations -> String)
;; produces a string of the resulting letters after applying the operations
#|
 CROSS PRODUCT OF TYPE-COMMENTS TABLE

        \               |                |
            \     l->   |     empty      |              String
    ops         \       |                |
 ____V_____________\____|________________|__________________________________
                        |                |
        empty           |      ""        |   (string-append (first l) ;(2)
                        |                |     (decoder (rest l) empty))
 _______________________|                |__________________________________   
                        |                |
       "keep"           |      ""        |   (string-append (first l) ;(3)
                        |                |     (decoder (rest l) (rest ops)))
 _______________________|      ;(1)      |__________________________________
                        |                | 
       "space"          |      ""        |   (string-append " " ;(4)
                        |                |     (decoder (rest l) (rest ops)))
 _______________________|                |__________________________________
                        |                |
       "remove"         |      ""        |   (decoder (rest l) (rest ops)) ;(5)
                        |                |   
                        |                |
|#

(check-expect (decoder empty empty) "")
(check-expect (decoder empty (list "keep")) "")
(check-expect (decoder empty (list "space")) "")
(check-expect (decoder empty (list "remove")) "")
(check-expect (decoder (list "a" "b" "c") empty) "abc")
(check-expect (decoder (list "a") empty) "a")
(check-expect (decoder (list "a" "b" "c") (list "keep")) "abc")
(check-expect (decoder (list "a") (list "keep")) "a")
(check-expect (decoder (list "a") (list "space")) " ")
(check-expect (decoder (list "a") (list "remove")) "")
(check-expect (decoder (list "i" "k" "l" "h" "a" "m" "k" "s" "a" "r" "m")
                       (list "keep" "remove" "remove" "space" "keep" "keep"
                             "space" "keep" "keep" "remove"))
              "i am sam")
(check-expect (decoder (list "f" "a" "d" "e")
                       (list "remove" "keep" "space" "keep" "remove"
                             "keep" "space"))
              "a e")

;(define (decoder l ops) "") ;stub

(@template 2-one-of)
(define (decoder l ops)
  (cond [(empty? l) ""] ;(1)
        [(empty? ops)  ;(2)
         (string-append (first l)
                        (decoder (rest l) empty))]
        [(string=? (first ops) "keep") ;(3)
         (string-append (first l)
                        (decoder (rest l) (rest ops)))]
        [(string=? (first ops) "space") ;(4)
         (string-append " "
                        (decoder (rest l) (rest ops)))]
        [else ;(5)
         (decoder (rest l) (rest ops))]))








;;
;; Problems 2-7.
;; Below is our solution for problem one in Problem Set 6.  Problems
;; 2-7 are included in this, asking you to improve the PS6 solution
;; in various ways.
;;

(@htdd Course)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth,
;;         a list of courses that have this course as a pre-requisite


(@htdd ListOfCourse)
;; ListOfCourse is one of:
;; - empty
;; - (cons Course ListOfCourse)
;; interp. a list of courses

(define LOC0 empty)
(define C322 (make-course 322 3 LOC0))
(define C320 (make-course 320 3 LOC0))
(define C319 (make-course 319 4 LOC0))
(define C317 (make-course 317 3 LOC0))
(define C314 (make-course 314 3 LOC0))
(define C313 (make-course 313 3 LOC0))
(define C312 (make-course 312 3 LOC0))
(define C311 (make-course 311 3 LOC0))
(define LOC1 (list C319))
(define C310 (make-course 310 3 LOC1))
(define C304 (make-course 304 3 LOC0))
(define C302 (make-course 302 3 LOC0))
(define C303 (make-course 303 3 LOC0))
(define LOC2 (list C304 C313 C314 C317 C320 C322))
(define C221 (make-course 221 4 LOC2))
(define LOC3 (list C313 C317))
(define C213 (make-course 213 4 LOC3))
(define LOC4 (list C213 C221 C310 C311 C312))
(define C210 (make-course 210 4 LOC4))
(define C203 (make-course 203 3 LOC0))
(define C189 (make-course 189 1 LOC0))
(define LOC5 (list C189 C203 C210 C302 C303))
(define C110 (make-course 110 4 LOC5))
(define C100 (make-course 100 3 LOC0))

(@problem 2)     
;;
;; Use local to refactor the two function templates below into  
;; a single encapsulated template that operates on a course.
;;

(define (fn-for-course c0)
  (local [(define (fn-for-course c)
            (... (course-number c)
                 (course-credits c)
                 (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) (...)]
                  [else
                   (... (fn-for-course (first loc))
                        (fn-for-loc (rest loc)))]))]
    (fn-for-course c0)))

(@problem 3)
;;
;; Refactor the following function designs into a design for a single
;; top-level function called all-course-numbers.
;;

(@htdf all-course-numbers)
(@signature Course -> ListOfNatural)
;; produce a list of all course numbers in given tree
(check-expect (all-course-numbers C100) (list 100))
(check-expect (all-course-numbers C213) (list 213 313 317))
(check-expect (all-course-numbers C210)
              (list 210 213 313 317 221 304 313
                    314 317 320 322 310 319 311 312))

(define (all-course-numbers c0)
  (local [(define (all-course-numbers--course c)
            (cons (course-number c)
                  (all-course-numbers--loc (course-dependents c))))

          (define (all-course-numbers--loc loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (all-course-numbers--course (first loc))
                           (all-course-numbers--loc (rest loc)))]))]

    (all-course-numbers--course c0)))

(@problem 4)
;; 
;; Refactor the following function designs into a design for a single
;; top-level function called courses-w-credits.
;;

(@htdf courses-w-credits)
(@signature Course Natural -> ListOfCourse)
;; produce list of courses in tree that have >= credits
(check-expect (courses-w-credits C100 4) empty)
(check-expect (courses-w-credits C100 3) (list C100))
(check-expect (courses-w-credits C100 2) (list C100))
(check-expect (courses-w-credits C110 3)
              (list C110 C203 C210 C213 C313 C317 C221 C304 C313 C314 C317 C320
                    C322 C310 C319 C311 C312 C302 C303))

(define (courses-w-credits c0 credits0)
  (local [(define (courses-w-credits--course c credits)
            (if (>= (course-credits c) credits)
                (cons c (courses-w-credits--loc (course-dependents c) credits))
                (courses-w-credits--loc (course-dependents c) credits)))

          (define (courses-w-credits--loc loc credits)
            (cond [(empty? loc) empty]
                  [else
                   (append (courses-w-credits--course (first loc) credits)
                           (courses-w-credits--loc (rest loc) credits))]))]

    (courses-w-credits--course c0 credits0)))



(@problem 5)
;;
;; Refactor the following function designs into a design for a single
;; top-level function called max-course-num.
;;

(@htdf max-course-num)
(@signature Course -> Natural)
;; produce the maximum course number a course in the tree has
;; CONSTRAINT: for max-course-num--loc, given list has at least 1 element
(check-expect (max-course-num C100) 100)
(check-expect (max-course-num C110) 322)

(define (max-course-num c0)
  (local [(define (max-course-num--course c)
            (max (course-number c)
                 (max-course-num--loc (course-dependents c))))

          (define (max-course-num--loc loc)
            (cond [(empty? loc) 0]
                  [else
                   (max (max-course-num--course (first loc))
                        (max-course-num--loc (rest loc)))]))]

    (max-course-num--course c0)))



(@problem 6)
;;
;; Refactor the following function designs into a design for a single
;; top-level function called find-course. Make appropriate use of local
;; elsewhere in the code as well.
;;

(@htdf find-course)
(@signature Course Natural -> Course or false)
;; produce course in tree with course-num, false if can't find
(check-expect (find-course C189 189) C189)
(check-expect (find-course C189 210) false)
(check-expect (find-course C110 310) C310)
(check-expect (find-course C110 349) false)

(define (find-course c0 course-num)
  (local [(define (find-course--course c)
            (if (= (course-number c) course-num)
                c
                (find-course--loc (course-dependents c))))

          (define (find-course--loc loc)
            (cond [(empty? loc) false]
                  [else
                   (local [(define try
                             (find-course--course (first loc)))]
                     (if (not (false? try))
                         try
                         (find-course--loc (rest loc))))]))]

    (find-course--course c0)))


(@problem 7)
;;
;; Design a function that consumes a course and produces the numbers of 
;; all courses in the tree that:
;;    have an odd course number, and
;;    are not pre-requisites for any other courses in the tree, and
;;    are worth at least 3 credits.
;;
;; You should produce a design for a single top-level  function called
;; courses-criteria.
;;

(@htdf courses-criteria)
(@signature Course -> ListOfNatural)
;; produces course where num is odd, credit>=3, and not a pre-req
(check-expect (courses-criteria C189) empty)
(check-expect (courses-criteria C303) (list 303))
(check-expect (courses-criteria C319) (list 319))
(check-expect (courses-criteria C302) empty)
(check-expect (courses-criteria C213) (list 313 317))
(check-expect (courses-criteria C110)
              (list 203 313 317 313 317 319 311 303))

;(define (courses-criteria c) empty) ;stub

(@template 2-one-of)
(define (courses-criteria c0)
  (local [(define (courses-criteria--course c)
            (if (meets-criteria? c)
                (cons (course-number c)
                      (courses-criteria--loc (course-dependents c)))
                (courses-criteria--loc (course-dependents c))))

          (define (courses-criteria--loc loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (courses-criteria--course (first loc))
                           (courses-criteria--loc (rest loc)))]))

          (define (meets-criteria? c)
            (and (odd? (course-number c))
                 (>= (course-credits c) 3)
                 (empty? (course-dependents c))))]
    
    (courses-criteria--course c0)))
