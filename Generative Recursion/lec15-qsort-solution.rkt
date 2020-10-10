;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lec17-qsort-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

;; Please design a function that does quicksort

(@htdf qsort)
(@signature (listof Number) -> (listof Number))
;; use quicksort to produce list of numbers in ascending order
;; CONSTRAINT there are no duplicates
(check-expect (qsort (list)) (list))
(check-expect (qsort (list 2)) (list 2))
(check-expect (qsort (list 5 4 6)) (list 4 5 6))
(check-expect (qsort (list 3 1 5 2 4 6)) (list 1 2 3 4 5 6))
(check-expect (qsort (list 8 3 11 1 12 5 2 10 4 6))
              (list 1 2 3 4 5 6 8 10 11 12))

(@template genrec use-abstract-fn encapsulated)

(define (qsort lon)
  ;; Base case: empty
  ;; Reduction: strict subset of lon
  ;; Argument: repeated strict subset of list always reaches empty
  (cond [(empty? lon) empty]
        [else
         (local [(define p (first lon))
                 (define (<p x) (< x p))
                 (define (>p x) (> x p))]
           
           (append (qsort (filter <p lon))
                   (list p)
                   (qsort (filter >p lon))))]))

#;  ;this version uses lambda
(define (qsort lon)
  ;; Base case: empty
  ;; Reduction: strict subset of lon
  ;; Argument: repeated strict subset of list always reaches empty
  (cond [(empty? lon) empty]
        [else
         (local [(define p (first lon))]
           (append (qsort (filter (lambda (x) (< x p)) lon))
                   (list p)
                   (qsort (filter (lambda (x) (> x p))  lon))))]))



