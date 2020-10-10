;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-10-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment pset-10); Do not edit or remove this tag
(@cwl ??? ???)       ; Replace ??? with your cwl,
;;                   ; second ??? is replaced with partner cwl if you have one



(@problem 1)

(@htdf max-num-repeats)
(@signature (listof String) -> Natural)
;; produce maximum number of times  same string appears consecutively in los0
(check-expect (max-num-repeats empty) 0)
(check-expect (max-num-repeats (list "cat")) 1)
(check-expect (max-num-repeats (list "cat" "bird" "dog")) 1)
(check-expect (max-num-repeats (list "cat" "cat" "bird" "dog")) 2)
(check-expect (max-num-repeats (list "cat" "cat" "bird" "dog" "dog" "dog"))
              3)
(check-expect (max-num-repeats (list "cat" "cat" "cat"
                                     "bird"
                                     "boy" "boy" "boy"
                                     "toy" "toy" "toy" "toy" "toy"
                                     "trick"
                                     "zebra" "zebra" "zebra" "zebra"))
              5)

(@template (listof String) accumulator)

(define (max-num-repeats los0)
  ;; prev is String: the previous first string in list
  ;; curr is Natural: number of times prev has been seen so far
  ;; rsf is Natural: maximum number of times a string has been seen so far
  (local [(define (fn-for-los los prev curr rsf)
            (cond [(empty? los) (max curr rsf)]
                  [else
                   (if (string=? (first los) prev)
                       (fn-for-los (rest los) (first los) (add1 curr) rsf)
                       (fn-for-los (rest los) (first los) 1 (max curr rsf)))]))]
    (if (empty? los0)
        0
        (fn-for-los (rest los0) (first los0) 1 0))))

(@problem 2)

(@htdf list-range)
(@signature (listof Integer) -> Natural)
;; produce the difference between the max and min integer in the list
;; CONSTRAINT: loi0 has at least one element
(check-expect (list-range (list 100)) 0)
(check-expect (list-range (list 2 -5 -10 50 80)) 90)
(check-expect (list-range (list 5000 -5 -100 50 0)) 5100)
(check-expect (list-range (list 3 8 1 2 9 4 2 3 -5)) 14)
(check-expect (list-range (list -5000 3 2 2 4 5000 4 2 3)) 10000)
(check-expect (list-range (list 400 500 500 400)) 100)

(@template (listof Integer) accumulator)
 
(define (list-range loi)
  ;; max-rsf is Integer: maximum integer seen so far
  ;; min-rsf is Integer: minimum integer seen so far
  (local [(define (fn-for-loi loi max-rsf min-rsf)
            (cond [(empty? loi) (- max-rsf min-rsf)]
                  [else
                   (fn-for-loi (rest loi)
                               (max (first loi) max-rsf)
                               (min (first loi) min-rsf))]))]
    (fn-for-loi (rest loi) (first loi) (first loi))))



(@problem 3)

(@htdf in-alphabetical-order?)
(@signature (listof String) -> Boolean)
;; produce true if list is sorted in order according to string-ci<=?
(check-expect (in-alphabetical-order? empty) true)
(check-expect (in-alphabetical-order? (list "a")) true)
(check-expect (in-alphabetical-order? (list "All" "bees" "Feel" "HAPPY")) true)
(check-expect (in-alphabetical-order? (list "aaaaa" "bb" "dd" "ccc")) false)

(@template (listof String) accumulator)

(define (in-alphabetical-order? los0)
  ;; prev is String: previous string in los
  (local [(define (fn-for-los los prev)
            (cond [(empty? los) true]
                  [else
                   (if (string-ci<=? prev (first los))
                       (fn-for-los (rest los) (first los))
                       false)]))]
    (if (empty? los0)
        true
        (fn-for-los (rest los0) (first los0)))))



;;
;; Please read through the data definition introduced in Problem Set 6
;; for a Course.
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





(@problem 4)

(@htdf filter-credits-course-num)
(@signature Course Natural Natural -> (listof Course))
;; produce list of courses with >= num-credits and number >= course-num
(check-expect (filter-credits-course-num C100 50 3) (list C100))
(check-expect (filter-credits-course-num C100 100 3) (list C100))
(check-expect (filter-credits-course-num C100 210 3) empty)
(check-expect (filter-credits-course-num C100 100 2) (list C100))
(check-expect (filter-credits-course-num C100 100 4) empty)
(check-expect (filter-credits-course-num C213 300 3) (list C317 C313))
(check-expect (filter-credits-course-num C110 200 4) (list C319 C221 C213 C210))

(@template Course ListOfCourse accumulator)
            

(define (filter-credits-course-num c0 course-num num-credits)
  ;; todo is (listof Course): worklist
  ;; rsf is (listof Course): list of courses satisfying criteria so far
  (local [(define (fn-for-course c todo rsf)
            (fn-for-loc (append (course-dependents c) todo)
                        (if (meets-requirements? c)
                            (cons c rsf)
                            rsf)))

          (define (fn-for-loc todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (fn-for-course (first todo) (rest todo) rsf)]))
	  
          (define (meets-requirements? c)
            (and (>= (course-credits c) num-credits)
                 (>= (course-number c) course-num)))]
    (fn-for-course c0 empty empty)))


(@problem 5)

(@htdf max-credit-path)
(@signature Course -> (listof Natural))
;; produce course numbers in path starting at c0 w/ max total credits
(check-expect (max-credit-path C100) (list 100))
(check-expect (max-credit-path C320) (list 320))
(check-expect (max-credit-path C213) (list 213 313))
(check-expect (max-credit-path C302) (list 302))
(check-expect (max-credit-path C221) (list 221 304))
(check-expect (max-credit-path C110) (list 110 210 213 313))

(@template Course ListOfCourse accumulator)

(define (max-credit-path c0)
  ;; todo is (listof WLE): worklist
  ;; path is (listof Course): courses on current path in the tree
  ;; rsf is (listof Course): max credit path so far
  (local [(define-struct wle (c p))

          (define (fn-for-course c todo path rsf)
            (local [(define npath (cons c path))]
              (if (empty? (course-dependents c))
                  (fn-for-loc todo (max-path npath rsf))
                  (fn-for-loc (append (map (lambda (s)
                                             (make-wle s npath))
                                           (course-dependents c))
                                      todo)
                              rsf))))

          (define (fn-for-loc todo rsf)
            (cond [(empty? todo) (reverse (map course-number rsf))]
                  [else
                   (fn-for-course (wle-c (first todo))
                                  (rest todo)
                                  (wle-p (first todo))
                                  rsf)]))

          (define (max-path p1 p2)
            (if (> (path-credits p1) (path-credits p2))
                p1
                p2))

          ;; (@signature (listof Course) -> Natural)
          ;; (@template fn-composition use-abstract-fn)
          (define (path-credits p)
            (foldr + 0 (map course-credits p)))]
    
    (fn-for-course c0 empty empty empty)))
                             
