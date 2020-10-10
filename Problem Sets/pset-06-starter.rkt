;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pset-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment pset-06); Do not edit or remove this tag
(@cwl atan12 ???)       ;Replace ??? with your cwl,
;;                   ;second ??? is replaced with partner cwl if you have one


(@problem 1)
;; Below is the start of a data definition called Course that represents limited
;; information about UBC courses.  Below there are only two example data.  
;; Please complete this definition by adding constants C110, C213, C313 and C317
;; which are representations of the descendent tree for 110, 213, 313 and 317.  
;; You can find the information you need at
;;  https://cs110.students.cs.ubc.ca/psets/pset-06-image.png
;;
;; NOTE 1: Use the information in the image above, rather than any other source.
;;         We are significantly simplying the information.
;;
;; NOTE 2: Do this very carefully, the autograder wants to see correct results
;;         from the functions you design to operate on this data.
;;
;; NOTE 3: The tree you will make for C110 will be a bit odd because 210 has 110
;;         as a pre-req, and both 213 and 221 have 210 as a pre-req, and313 has
;;         213 AND 221 as a pre-req, and 317 has 213 AND 221 as a pre-req. As a
;;         result, 313 and 317 will both show up twice in your descendent tree
;          for C110. This is okay for this problem set.
;; NOTE 4: Expect this step of the problem set to take you some time.


(@htdd Course)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth, and a
;;         list of courses that list this course as a direct pre-requisite


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
(define C310 (make-course 310 4 LOC1))
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

(define (fn-for-course c)
  (... (course-number c)
       (course-credits c)
       (fn-for-loc (course-dependents c))))

(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-course (first loc))
              (fn-for-loc (rest loc)))]))

(@problem 2)
;;
;; Design a function that produces the list of all the course numbers in the
;; course's tree including the given course's number.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf all-course--course all-course--loc)
(@signature Course -> ListOfNatural)
(@signature ListOfCourse -> ListOfNatural)
;; produce the list of all the course numbers
(check-expect (all-course--loc empty) empty)
(check-expect (all-course--loc LOC1) (list 319))
(check-expect (all-course--loc LOC5) (list 189 203 210 213 313
                                           317 221 304 313 314 317
                                           320 322 310 319 311 312 302 303))
(check-expect (all-course--course C322) (list 322))
(check-expect (all-course--course C110) (list 110 189 203 210 213 313
                                              317 221 304 313 314 317
                                              320 322 310 319 311 312 302 303))
(check-expect (all-course--course C213) (list 213 313 317))

;(define (all-course--course c) empty) ;stubs
;(define (all-course--loc loc) empty)

(@template Course)
(define (all-course--course c)
  (cons (course-number c)
        (all-course--loc (course-dependents c))))

(@template ListOfCourse)
(define (all-course--loc loc)
  (cond [(empty? loc) empty]
        [else
         (append (all-course--course (first loc))
                 (all-course--loc (rest loc)))]))

(@problem 3)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the list of courses in the tree that are worth that
;; many credits or more.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf course-with-credit--course course-with-credit--loc)
(@signature Course Natural -> ListOfCourse)
(@signature ListOfCourse Natural -> ListOfCourse)
;; produce the list of courses that are worth that many credits or more
(check-expect (course-with-credit--loc empty 2) empty)
(check-expect (course-with-credit--loc LOC2 3) (list C304 C313 C314
                                                     C317 C320 C322))
(check-expect (course-with-credit--loc LOC2 4) empty)
(check-expect (course-with-credit--course C322 3) (list C322))
(check-expect (course-with-credit--course C322 4) empty)
(check-expect (course-with-credit--course C110 3) (list C110 C203 C210 C213 C313
                                                        C317 C221 C304 C313 C314
                                                        C317 C320 C322 C310 C319
                                                        C311 C312 C302 C303))
(check-expect (course-with-credit--course C110 4) (list C110 C210 C213
                                                        C221 C310 C319))

;(define (course-with-credit--course c cr) empty) ;stubs
;(define (course-with-credit--loc loc cr) empty)

(@template Course)
(define (course-with-credit--course c cr)
  (if (>= (course-credits c) cr)
      (cons c (course-with-credit--loc (course-dependents c) cr))
      (course-with-credit--loc (course-dependents c) cr)))

(@template ListOfCourse)
(define (course-with-credit--loc loc cr)
  (cond [(empty? loc) empty]
        [else
         (append (course-with-credit--course (first loc) cr)
                 (course-with-credit--loc (rest loc) cr))]))

(@problem 4)
;;
;; Design a function that produces the largest course number in the tree.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf largest--course largest--loc)
(@signature Course -> Natural)
(@signature ListOfCourse -> Natural)
;; produces the largest course number in the tree
(check-expect (largest--course C100) 100)
(check-expect (largest--loc (list C100)) 100)
(check-expect (largest--loc (list C317 C319)) 319)
(check-expect (largest--course C110) 322)
(check-expect (largest--loc LOC5) 322)

;(define (largest--course c) c) ;stubs
;(define (largest--loc loc) (make-course 0 0 LOC0))

(@template Course)

(define (largest--course c)
  (max (course-number c)
       (largest--loc (course-dependents c))))

(@template ListOfCourse)

(define (largest--loc loc)
  (cond [(empty? loc) 0]
        [else
         (max (largest--course (first loc))
              (largest--loc (rest loc)))]))

(@problem 5)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the course in the tree with that course number. If it
;; can't find a course in the given tree with that course number, it signals
;; failure by producing false.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf find--course find--loc)
(@signature Course Natural -> Course or false)
(@signature ListOfCourse Natural -> Course or false)
;; produce the course in the tree with that course number, otherwise false
(check-expect (find--loc empty 110) false)
(check-expect (find--loc LOC5 110) false)
(check-expect (find--loc LOC5 210) C210)
(check-expect (find--course C110 322) C322)
(check-expect (find--course C110 121) false)
(check-expect (find--course C322 322) C322)
(check-expect (find--course C322 319) false)

;(define (find--course c n) false) ;stubs
;(define (find--loc loc n) false)

(@template backtracking Course)
(define (find--course c n)
  (if (= (course-number c) n)
      c
      (find--loc (course-dependents c) n)))

(@template backtracking ListOfCourse)
(define (find--loc loc n)
  (cond [(empty? loc) false]
        [else
         (if (not (false? (find--course (first loc) n)))
             (find--course (first loc) n)
             (find--loc (rest loc) n))]))