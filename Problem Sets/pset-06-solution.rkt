;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pset-06-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment pset-06); Do not edit or remove this tag
(@cwl ??? ???)       ;Replace ??? with your cwl,
;;                   ;second ??? is replaced with partner cwl if you have one


(@problem 1)

(@htdd Course)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth, and a
;;         list of courses that have this course as a pre-requisite


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

(@htdf all-course-numbers--course all-course-numbers--loc)
(@signature Course -> ListOfNatural)
(@signature ListOfCourse -> ListOfNatural)
;; produce a list of all course numbers in given tree
(check-expect (all-course-numbers--loc empty) empty)
(check-expect (all-course-numbers--course C100) (list 100))
(check-expect (all-course-numbers--loc LOC3) (list 313 317))
(check-expect (all-course-numbers--course C213) (list 213 313 317))
(check-expect (all-course-numbers--loc LOC4)
              (list 213 313 317 221 304 313 314 317 320 322 310 319 311 312))
(check-expect (all-course-numbers--course C210)
              (list 210 213 313 317 221 304 313
                    314 317 320 322 310 319 311 312))

(@template Course)

(define (all-course-numbers--course c)
  (cons (course-number c)
        (all-course-numbers--loc (course-dependents c))))

(@template ListOfCourse)

(define (all-course-numbers--loc loc)
  (cond [(empty? loc) empty]
        [else
         (append (all-course-numbers--course (first loc))
                 (all-course-numbers--loc (rest loc)))]))

(@problem 3)

(@htdf courses-w-credits--course courses-w-credits--loc)
(@signature Course Natural -> ListOfCourse)
(@signature ListOfCourse Natural -> ListOfCourse)
;; produce list of courses in tree that have >= credits
(check-expect (courses-w-credits--loc empty 4) empty)
(check-expect (courses-w-credits--course C100 4) empty)
(check-expect (courses-w-credits--course C100 3) (list C100))
(check-expect (courses-w-credits--course C100 2) (list C100))
(check-expect (courses-w-credits--loc (list C313 C189 C319) 3)
              (list C313 C319))
(check-expect (courses-w-credits--course C110 3)
              (list C110 C203 C210 C213 C313 C317 C221 C304 C313 C314 C317 C320
                    C322 C310 C319 C311 C312 C302 C303))
(check-expect (courses-w-credits--loc LOC4 4) (list C213 C221 C310 C319))

(@template Course)

(define (courses-w-credits--course c credits)
  (if (>= (course-credits c) credits)
      (cons c (courses-w-credits--loc (course-dependents c) credits))
      (courses-w-credits--loc (course-dependents c) credits)))

(@template ListOfCourse)

(define (courses-w-credits--loc loc credits)
  (cond [(empty? loc) empty]
        [else
         (append (courses-w-credits--course (first loc) credits)
                 (courses-w-credits--loc (rest loc) credits))]))

(@problem 4)

(@htdf max-course-num--course max-course-num--loc)
(@signature Course -> Natural)
(@signature ListOfCourse -> Natural)
;; produce the maximum course number a course in the tree has
(check-expect (max-course-num--course C100) 100)
(check-expect (max-course-num--loc (list C100)) 100)
(check-expect (max-course-num--loc (list C317 C319)) 319)
(check-expect (max-course-num--course C110) 322)
(check-expect (max-course-num--loc LOC5) 322)

(@template Course)

(define (max-course-num--course c)
  (max (course-number c)
       (max-course-num--loc (course-dependents c))))

(@template ListOfCourse)

(define (max-course-num--loc loc)
  (cond [(empty? loc) 0]
        [else
         (max (max-course-num--course (first loc))
              (max-course-num--loc (rest loc)))]))

(@problem 5)

(@htdf find-course--course find-course--loc)
(@signature Course Natural -> Course or false)
(@signature ListOfCourse Natural -> Course or false)
;; produce course in tree with given course number, false otherwise
(check-expect (find-course--loc empty 110) false)
(check-expect (find-course--course C189 189) C189)
(check-expect (find-course--course C189 210) false)
(check-expect (find-course--loc LOC3 317) C317)
(check-expect (find-course--loc LOC5 313) C313)
(check-expect (find-course--course C110 310) C310)
(check-expect (find-course--course C110 349) false)

(@template Course backtracking)

(define (find-course--course c course-num)
  (if (= (course-number c) course-num)
      c
      (find-course--loc (course-dependents c) course-num)))

(@template ListOfCourse backtracking)

(define (find-course--loc loc course-num)
  (cond [(empty? loc) false]
        [else
         (if (not (false? (find-course--course (first loc) course-num)))
             (find-course--course (first loc) course-num)
             (find-course--loc (rest loc) course-num))]))
