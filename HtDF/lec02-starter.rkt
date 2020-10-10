;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lec02-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)


(@problem 1)
;;
;; Design a function, called topple, that takes an image and rotates it 
;; by 90 degrees.
;;

;;Image -> Image
;;Produces the inputted image rotated by 90 degrees

(check-expect (topple (rectangle 10 20 "solid" "red"))
              (rectangle 20 10 "solid" "red"))
(check-expect (topple (triangle 20 "solid" "red"))
              (rotate 90 (triangle 20 "solid" "red")))

#;
(define (topple img) 0) ;stub

#;
(define (topple img) ;template
  (... img))

(define (topple img)
  (rotate 90 img))


(@problem 2)
;;
;; Design a function that consumes the name of something and produces a
;; "checkbox line" image that allows someone to check off that item.  For 
;; example (checkbox-line "apples") would produce an image with a small
;; check box next to the word apples.
;;

;; String -> Image
;; Produces a "checkbox line" image next to the string

(check-expect (checkbox-line "apples")
              (beside (square 30 "outline" "black")
                      (text "apples" 30 "black")))

(check-expect (checkbox-line "oranges")
              (beside (square 30 "outline" "black")
                      (text "oranges" 30 "black")))

#;
(define (checkbox-line str) 0)

(define (checkbox-line str)
  (beside (square 30 "outline" "black") (text str 30 "black")))

(@problem 3)
;;
;; Design a function, that consumes an image and determines whether it is tall.
;;

;; Image -> Boolean
;; Produces true if the image is tall

(check-expect (tall? (rectangle 10 20 "outline" "black")) true)
(check-expect (tall? (rectangle 10 10 "outline" "black")) false)
(check-expect (tall? (rectangle 10 11 "outline" "black")) true)
(check-expect (tall? (rectangle 30 20 "outline" "black")) false)

#;
(define (tall? img) false)

(define (tall? img)
  (> (image-height img) (image-width img)))

(@problem 4)
;;
;; Design a function, called image>, that takes two images and determines 
;; whether the first is larger than the second.
;;

;; Image Image -> Boolean
;; Produces true if the first image is larger than the second

(check-expect (image> empty-image empty-image) false)
(check-expect (image> (rectangle 10 21 "outline" "black")
                      (rectangle 10 20 "outline" "black"))
              true)
(check-expect (image> (rectangle 10 20 "outline" "black")
                      (rectangle 10 20 "outline" "black"))
              false)
(check-expect (image> (rectangle 10 19 "outline" "black")
                      (rectangle 10 20 "outline" "black"))
              false)

#;
(define (image> img1 img2) false)

(define (image> img1 img2)
  (or (> (image-height img1) (image-height img2)) (> (image-width img1) (image-width img2))))





