;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname dropn-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require spd/tags)

(@assignment accumulators-p1)
(@cwl atan12)

(@problem 1)
;; Design a function that consumes a list of elements lox and a natural number
;; n and produces the list formed by dropping every nth element from lox.
;;
;; (dropn (list 1 2 3 4 5 6 7) 2) should produce (list 1 2 4 5 7)

(@htdf dropn)
(@signature (listof X) Natural -> (listof X))
;; produces the list formed by dropping every nth element from lox
(check-expect (dropn (list 1 2 3 4 5 6 7) 2) (list 1 2 4 5 7))
(check-expect (dropn (list true false true false) 1) (list true true))

(@template (listof X) encapsulated accumulator)
(define (dropn lox0 n)
  ;; cur is Natural ; the current element number from lox
  (local [(define (solve lox cur)
            (cond [(empty? lox) empty]
                  [else
                   (if (= cur n)
                       (solve (rest lox) 0)
                       (cons (first lox) (solve (rest lox) (add1 cur))))]))]
    (solve lox0 0)))

