;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |boolean and if expressions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;true
;false

(define WIDTH 100)
(define HEIGHT 100)

;predicates are primitives or functions that produce a boolean value (true or false)
(> WIDTH HEIGHT) ;false
(>= WIDTH HEIGHT) ;true

(string=? "foo" "bar") ;false, checks if two strings are equal

(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (rectangle 20 10 "solid" "blue"))

(if (< (image-width I1)
       (image-height I1))
    "tall"
    "wide")

(and (> (image-height I1) (image-height I2))
     (< (image-width I1) (image-height I2)) ;and function uses short circuiting (stops when it first sees false)


