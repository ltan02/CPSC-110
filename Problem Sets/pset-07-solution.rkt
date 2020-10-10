;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pset-07-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER.
;;
(require spd/tags)

(@assignment pset-07); Do not edit or remove this tag
(@cwl ??? ???)       ; Replace ??? with your cwl,
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
;;         "keep"   means keep the letter
;;         "space"  means replace the letter with a space
;;         "remove" means remove the letter

(@htdf decoder)
(@signature Letters Operations -> String)
;; produce string containing letters in l with operations applied
(check-expect (decoder empty empty) "")
(check-expect (decoder empty (list "keep")) "")
(check-expect (decoder empty (list "space")) "")
(check-expect (decoder empty (list "remove")) "")
(check-expect (decoder (list "h" "i") empty) "hi")
(check-expect (decoder (list "h" "o" "w") (list "keep" "keep" "keep"))
              "how")
(check-expect (decoder (list "o" "h") (list "space" "space"))
              "  ")
(check-expect (decoder (list "a" "d" "d") (list "remove" "remove"))
              "d")
(check-expect (decoder (list "i" "k" "l" "h" "a" "m" "k" "s" "a" "r" "m")
                       (list "keep" "remove" "remove" "space" "keep" "keep"
                             "space" "keep" "keep" "remove" "keep"))
              "i am sam")

(check-expect (decoder (list "e" "s" "r" "a" "m" "d" "i" "f" "a" "m")
                       (list "remove" "keep" "remove" "keep" "keep" "space"
                             "keep" "space"))
              "sam i am")


#|
CROSS PRODUCT OF TYPE-COMMENTS TABLE

       \             |                |
           \     l-> |   empty        |  (cons String Letters)
   ops         \     |                |
____V_____________\__|________________|__________________________________
                     |                |
   empty             |                | (string-append (first l)        ;(2)
                     |                |                (decoder (rest l)
                     |                |                         empty))
_____________________|                |_____________________________________
                     |                |
 (cons "keep"        |                | (string-append (first l)        ;(3)
       Operations)   |                |                (decoder (rest l)
                     |                |                         (rest ops)))
_____________________|     "" ;(1)    |_____________________________________
                     |                |
 (cons "space"       |                | (string-append " "             ;(4)
       Operations)   |                |                (decoder (rest l)
                     |                |                         (rest ops)))
_____________________|                |_____________________________________
                     |                |
 (cons "remove"      |                | (decoder (rest l) (rest ops))  ;(5)
       Operations)   |                |
|#

(@template 2-one-of)

(define (decoder l ops)
  (cond [(empty? l) ""]                       ;(1)
        [(empty? ops)                         ;(2)
         (string-append (first l)
                        (decoder (rest l)
                                 empty))]
        [(string=? "keep" (first ops))        ;(3)
         (string-append (first l)
                        (decoder (rest l)
                                 (rest ops)))]
        [(string=? "space" (first ops))       ;(4)
         (string-append " "
                        (decoder (rest l)
                                 (rest ops)))]
        [else                                 ;(5)
         (decoder (rest l)
                  (rest ops))]))


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

(@template Course ListOfCourse encapsulated)

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

(@htdf all-course-numbers)
(@signature Course -> ListOfNatural)
;; produce a list of all course numbers in given tree
(check-expect (all-course-numbers C100) (list 100))
(check-expect (all-course-numbers C213) (list 213 313 317))
(check-expect (all-course-numbers C210)
              (list 210 213 313 317 221 304 313
                    314 317 320 322 310 319 311 312))

(@template Course ListOfCourse encapsulated)

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

(@htdf courses-w-credits)
(@signature Course Natural -> ListOfCourse)
;; produce list of courses in tree that have >= credits
(check-expect (courses-w-credits C100 4) empty)
(check-expect (courses-w-credits C100 3) (list C100))
(check-expect (courses-w-credits C100 2) (list C100))
(check-expect (courses-w-credits C110 3)
              (list C110 C203 C210 C213 C313 C317 C221 C304 C313 C314 C317 C320
                    C322 C310 C319 C311 C312 C302 C303))

(@template Course ListOfCourse encapsulated)

(define (courses-w-credits c0 credits)
  (local [(define (courses-w-credits--course c)
            (if (>= (course-credits c) credits)
                (cons c (courses-w-credits--loc (course-dependents c)))
                (courses-w-credits--loc (course-dependents c))))

          (define (courses-w-credits--loc loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (courses-w-credits--course (first loc))
                           (courses-w-credits--loc (rest loc)))]))]
    (courses-w-credits--course c0)))



(@problem 5)

(@htdf max-course-num)
(@signature Course -> Natural)
;; produce the maximum course number a course in the tree has
(check-expect (max-course-num C100) 100)
(check-expect (max-course-num C110) 322)

(@template Course ListOfCourse encapsulated)

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

(@htdf find-course)
(@signature Course Natural -> Course or false)
;; produce course in tree with course-num, false if can't find
(check-expect (find-course C189 189) C189)
(check-expect (find-course C189 210) false)
(check-expect (find-course C110 310) C310)
(check-expect (find-course C110 349) false)

(@template Course ListOfCourse backtracking encapsulated)

(define (find-course c course-num)
  (local[(define (find-course--course c)
           (if (= (course-number c) course-num)
               c
               (find-course--loc (course-dependents c))))

         (define (find-course--loc loc)
           (cond [(empty? loc) false]
                 [else
                  (local [(define try (find-course--course (first loc)))]
                    (if (not (false? try))
                        try
                        (find-course--loc (rest loc))))]))]
    (find-course--course c)))



(@problem 7)

(@htdf courses-criteria)
(@signature Course -> ListOfNatural)
;; produce courses for which num is odd, credits = 3, and course is not pre-req
(check-expect (courses-criteria C189) empty)
(check-expect (courses-criteria C303) (list 303))
(check-expect (courses-criteria C319) (list 319))
(check-expect (courses-criteria C302) empty)
(check-expect (courses-criteria C213) (list 313 317))
(check-expect (courses-criteria C110)
              (list 203 313 317 313 317 319 311 303))

(@template Course ListOfCourse encapsulated)

(define (courses-criteria c0)
  (local [(define (fn-for-course c)
            (if (meets-criteria? c)
                (cons (course-number c)
                      (fn-for-loc (course-dependents c)))
                (fn-for-loc (course-dependents c))))

	  #;
	  (define (fn-for-course c)
            (if (meets-criteria? c)
                (list (course-number c))
                (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (fn-for-course (first loc))
                           (fn-for-loc (rest loc)))]))

          ;; additional helper based on knowledge domain shift
          (define (meets-criteria? c)
            (and (odd? (course-number c))
                 (>= (course-credits c) 3)
                 (empty? (course-dependents c))))]
	 
    (fn-for-course c0)))

