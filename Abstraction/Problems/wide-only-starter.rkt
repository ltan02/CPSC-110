;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname wide-only-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require spd/tags)

(@assignment abstraction-p1)
(@cwl ??? ???)

(@problem 1)
;; Use the built in version of filter to design a function called wide-only 
;; that consumes a list of images and produces a list containing only those 
;; images that are wider than they are tall.

(@htdf wide-only)
(@signature (listof Image) -> (listof Image))
;; produces a list of images that are wider than they are tall
(check-expect (wide-only empty) empty)
(check-expect (wide-only (list (rectangle 10 20 "solid" "black"))) empty)
(check-expect (wide-only (list (rectangle 10 5 "solid" "blue")
                               (rectangle 5 10 "outline" "red")
                               (rectangle 15 2 "outline" "green")))
              (list (rectangle 10 5 "solid" "blue")
                    (rectangle 15 2 "outline" "green")))

;(define (wide-only loi) empty) ;stub

(@template use-abstract-fn)
(define (wide-only loi)
  (local [(define (wide? img)
            (> (image-width img) (image-height img)))]
    (filter wide? loi)))
