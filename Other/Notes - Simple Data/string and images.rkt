;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |string and images|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
"apple" ;examples of strings
"Ada"

(string-append "Ada" " " "Lovelace") ;append strings

"123" ;this is a string
123 ;this is a number

;"123" != 123

(string-length "apple") ;gets the length of a string

(substring "Caribou" 2 4) ;take out all the characters from 2 to 4 (exluding 4)

(require 2htdp/image) ;tells DrRacket that we will use image functions from the 2nd edition of the How to Design Programs book

(circle 10 "solid" "red") ;arg_1 = radius, arg_2 = solid or outline, arg_3 = colour

(rectangle 30 60 "outline" "blue") ;arg_1 = width, arg_2 = height

(text "hello" 24 "orange") ;text, font size, colour

(above (circle 10 "solid" "red")
       (circle 20 "solid" "yellow")
       (circle 30 "solid" "green")) ;takes all the arguements and stacks them on top of each other

(beside (circle 10 "solid" "red")
       (circle 20 "solid" "yellow")
       (circle 30 "solid" "green"))

(overlay (circle 10 "solid" "red")
       (circle 20 "solid" "yellow")
       (circle 30 "solid" "green"))