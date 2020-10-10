;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lecture 6 - Notes|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

empty

(cons "Flames" empty) ;a list of 1 element
(cons "Leafs" (cons "Flames" empty)) ;list of 2 elements

(cons (string-append "C" "anucks") empty)

(cons 10 (cons 9 (cons 10 empty))) ; a list of 3 elements

(cons (square 10 "solid" "blue")
      (cons (triangle 20 "solid" "green")
            empty))

(define L1 (cons "Flames" empty))
(define L2 (cons 10 (cons 9 (cons 10 empty))))
(define L3 (cons (square 10 "solid" "blue")
                 (cons (triangle 20 "solid" "green")
                       empty)))

(first L1)
(first L2)
(first L3)

; cons     a two arguement constructor
; first    selects the first elements of a list
; rest     consumes a list ith at least one element and produces the list after the first element

(rest L1)
(rest L2)
(rest L3)

(first (rest L2)) ; To get the second element of L2
(first (rest (rest L2))) ;To get the third element

(empty? empty) ;produces true
(empty? L1) ;produces false

; List Data Definition: quidditch-starter.rkt

; Designing with Lists: designing-with-lists-1-starter.rkt

; Positions in List Templates
