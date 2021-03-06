;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lec16-courses-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)


(@htdd Course)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural (listof Course))
;; interp. a course with a course number,
;;         the number of credits the course is worth,
;;         a list of courses that have this course as a pre-requisite

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

(@template Course (listof Course) encapsulated)

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



(@problem 1)
;;
;; Design an abstract fold function for Course.
;;

(@htdf fold-course)
(@signature (Natural Natural Y -> X) (X Y -> Y) Y -> X)
;; abstract fold for course
(check-expect (fold-course make-course cons empty C110) C110)


(@template Course (listof Course) encapsulated)

(define (fold-course c1 c2 b1 c0)
  (local [(define (fn-for-course c)  ;--> X
            (c1 (course-number c)
                (course-credits c)
                (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)   ;--> Y
            (cond [(empty? loc) b1]
                  [else
                   (c2 (fn-for-course (first loc))
                       (fn-for-loc (rest loc)))]))]

    (fn-for-course c0)))


(@problem 2)
;;
;; Complete the definition of this function to use fold-course.
;;

(@htdf all-course-numbers)
(@signature Course -> ListOfNatural)
;; produce a list of all course numbers in given tree
(check-expect (all-course-numbers C100) (list 100))
(check-expect (all-course-numbers C213) (list 213 313 317))
(check-expect (all-course-numbers C210)
              (list 210 213 313 317 221 304 313
                    314 317 320 322 310 319 311 312))

(@template use-abstract-fn)

(define (all-course-numbers c)
  (local [(define (num-and-rmr num credits rmr) (cons num rmr))]
    (fold-course num-and-rmr append empty c)))


(@problem 3)

;;
;; Complete the design of this function using fold-course.
;;

(@htdf total-credits)
(@signature Course -> Natural)
;; produce the total number of credits in a course tree
(check-expect (total-credits C100) 3)
(check-expect (total-credits C110) 63)

(@template use-abstract-fn)


(define (total-credits c)
  (local [(define (c1 number credits rmr) (+ credits rmr))
          (define (c2 rmr rnr) (+ rmr rnr))
          (define b1 0)]
    (fold-course c1 c2 b1 c)))



(@problem 4)
;;
;; Complete the design of the following function.
;;
(@htdf courses-w-credits)
(@signature Course Natural -> (listof Course))
;; produce list of courses in tree that have >= credits
(check-expect (courses-w-credits C100 4) empty)
(check-expect (courses-w-credits C100 3) (list C100))
(check-expect (courses-w-credits C100 2) (list C100))
(check-expect (courses-w-credits C110 3)
              (list C110 C203 C210 C213 C313 C317 C221 C304 C313 C314 C317 C320
                    C322 C310 C319 C311 C312 C302 C303))


#|
;; we can't do this with fold-course! instead we copy and edit the templates.
(@template use-abstract-fn)

(define (courses-w-credits c0 n)
  (local [(define (c1 number credits rmr)
            (if (>= credits n)
                (cons <<c>> rmr)      ;<<<don't have the course to put here!
                rmr))
          (define c2 append)
          (define b1 empty)]
    (fold-course c1 c2 b1 c0)))
|#

(@template Course (listof Course) encapsulated)

(define (courses-w-credits c0 n)
  (local [(define (fn-for-course c)
            (if (>= (course-credits c) n)
                (cons c (fn-for-loc (course-dependents c)))
                (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) empty]
                  [else
                   (append (fn-for-course (first loc))
                           (fn-for-loc (rest loc)))]))]

    (fn-for-course c0)))


(@problem 5)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the course in the tree with that course number. If it
;; can't find a course in the given tree with that course number, it produces
;; false.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be last.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf find-course)
(@signature Course Natural -> Natural or false)
;; search tree for course w/ given number; if found produce credits
(check-expect (find-course C189 189) 1)
(check-expect (find-course C189 210) false)
(check-expect (find-course C110 310) 3)
(check-expect (find-course C110 349) false)


#|
;; this produces the correct answer with the fold function, but it
;; searches the entire tree every time, rather than stopping when it
;; gets to the result it wants to find. So we should use the templates.
(@template use-abstract-fn)

(define (find-course c0 n)
  (local [(define (c1 num creds rmr)
            (if (= num n)
                creds
                rmr))
          (define (c2 rmr rnr)
            (if (not (false? rmr))
                rmr
                rnr))]

    (fold-course c1 c2 false c0)))
|#

(@template Course (listof Course) encapsulated)

(define (find-course c0 n)
  (local [(define (fn-for-course c)
            (if (= (course-number c) n)
                (course-credits c)
                (fn-for-loc (course-dependents c))))

          (define (fn-for-loc loc)
            (cond [(empty? loc) false]
                  [else
                   (local [(define try (fn-for-course (first loc)))]
                     (if (not (false? try))
                         try
                         (fn-for-loc (rest loc))))]))]

    (fn-for-course c0)))




